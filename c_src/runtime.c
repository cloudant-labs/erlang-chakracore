// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

#include <string.h>

#include "ChakraCore.h"
#include "erl_nif.h"

#include "atoms.h"
#include "erl2js.h"
#include "js2erl.h"
#include "runtime.h"
#include "util.h"


void* rt_loop(void* arg);
void erl_chakra_collect_serialized_object(JsValueRef obj, void* script);
bool erl_chakra_load_script(
        JsSourceContext src_ctx,
        JsValueRef* value,
        JsParseScriptAttributes* attrs
    );


ErlChakraJob*
erl_chakra_job_create()
{
    ErlChakraJob* ret = enif_alloc(sizeof(ErlChakraJob));

    ret->env = enif_alloc_env();
    ret->ref = enif_make_ref(ret->env);
    ret->type = ERL_CHAKRA_JOB_TYPE_UNKNOWN;
    ret->ctx = NULL;
    ret->args[0] = 0;
    ret->args[1] = 0;

    return ret;
}


void
erl_chakra_job_destroy(ErlChakraJob* job)
{
    if(job->ctx != NULL) {
        enif_release_resource(job->ctx);
    }

    enif_free_env(job->env);
    enif_free(job);
}


ERL_NIF_TERM
erl_chakra_runtime_create(
        ErlNifEnv* env,
        JsRuntimeAttributes attrs,
        int memory_limit,
        int stack_size
    )
{
    ErlChakraRt* rt = NULL;

    ErlNifThreadOpts* thr_opts = NULL;
    ERL_NIF_TERM ret;

    JsErrorCode err = JsNoError;

    rt = enif_alloc_resource(ErlChakraRtRes, sizeof(ErlChakraRt));
    rt->runtime = JS_INVALID_RUNTIME_HANDLE;
    rt->alive = false;

    err = JsCreateRuntime(attrs, NULL, &(rt->runtime));
    if(err != JsNoError) {
        rt->runtime = JS_INVALID_RUNTIME_HANDLE;
        goto error;
    }

    err = JsSetRuntimeMemoryLimit(rt->runtime, memory_limit);
    if(err != JsNoError) {
        goto error;
    }

    enif_self(env, &(rt->pid));

    rt->lock = enif_mutex_create(NULL);
    rt->cond = enif_cond_create(NULL);
    rt->job = NULL;

    if((attrs & JsRuntimeAttributeAllowScriptInterrupt) == 0) {
        rt->allow_script_interrupt = false;
    } else {
        rt->allow_script_interrupt = true;
    }

    thr_opts = enif_thread_opts_create(NULL);
    thr_opts->suggested_stack_size = stack_size;

    if(enif_thread_create(NULL, &(rt->tid), rt_loop, rt, thr_opts)) {
        goto error;
    }

    // Wait for the thread to start before returning
    // to avoid race conditions attempting to submit
    // a job before the thread is fully started.
    enif_mutex_lock(rt->lock);
    while(!rt->alive) {
        enif_cond_wait(rt->cond, rt->lock);
    }
    enif_mutex_unlock(rt->lock);

    ret = T2(env, ATOM_ok, enif_make_resource(env, rt));

error:
    if(thr_opts != NULL) {
        enif_thread_opts_destroy(thr_opts);
    }

    if(err != JsNoError) {
        js2erl_error(env, err, &ret);
    }

    enif_release_resource(rt);
    return ret;
}


void
erl_chakra_runtime_destroy(ErlChakraRt* rt)
{
    ErlChakraJob* job = erl_chakra_job_create();
    job->type = ERL_CHAKRA_JOB_TYPE_CLOSE;

    JsDisableRuntimeExecution(rt->runtime);

    enif_mutex_lock(rt->lock);
    while(rt->job != NULL) {
        enif_cond_wait(rt->cond, rt->lock);
    }
    rt->job = job;
    enif_cond_signal(rt->cond);
    enif_mutex_unlock(rt->lock);

    enif_thread_join(rt->tid, NULL);
    JsDisposeRuntime(rt->runtime);
}


bool
erl_chakra_runtime_submit(ErlChakraRt* rt, ErlChakraJob* job)
{
    bool ret;

    enif_mutex_lock(rt->lock);
    if(rt->job == NULL && rt->alive) {
        rt->job = job;
        ret = true;
        enif_cond_signal(rt->cond);
    } else {
        ret = false;
    }
    enif_mutex_unlock(rt->lock);

    return ret;
}


ERL_NIF_TERM
erl_chakra_rt_gc(ErlChakraRt* rt, ErlChakraJob* job)
{
    JsErrorCode err = JsCollectGarbage(rt->runtime);
    if(err != JsNoError) {
        return T2(job->env, ATOM_error, js2erl_error_code(err));
    }

    return ATOM_ok;
}


ERL_NIF_TERM
erl_chakra_rt_enable(ErlChakraRt* rt, ErlChakraJob* job)
{
    JsErrorCode err = JsEnableRuntimeExecution(rt->runtime);
    if(err != JsNoError) {
        return T2(job->env, ATOM_error, js2erl_error_code(err));
    }

    return ATOM_ok;
}


ERL_NIF_TERM
erl_chakra_rt_disable(ErlChakraRt* rt, ErlChakraJob* job)
{
    JsErrorCode err = JsDisableRuntimeExecution(rt->runtime);
    if(err != JsNoError) {
        return T2(job->env, ATOM_error, js2erl_error_code(err));
    }

    return ATOM_ok;
}


ERL_NIF_TERM
erl_chakra_rt_create_context(ErlChakraRt* rt, ErlChakraJob* job)
{
    ErlChakraCtx* ctx;
    ERL_NIF_TERM ret;
    JsErrorCode err;

    ctx = enif_alloc_resource(ErlChakraCtxRes, sizeof(ErlChakraCtx));
    ctx->rt = rt;
    ctx->context = JS_INVALID_REFERENCE;

    enif_keep_resource(rt);

    err = JsCreateContext(rt->runtime, &(ctx->context));
    if(err != JsNoError) {
        goto error;
    }

    err = JsAddRef(ctx->context, NULL);
    if(err != JsNoError) {
        goto error;
    }

    ret = enif_make_resource(job->env, ctx);
    enif_release_resource(ctx);

    return T2(job->env, ATOM_ok, ret);

error:
    ctx->context = JS_INVALID_REFERENCE;
    enif_release_resource(ctx);
    js2erl_error(job->env, err, &ret);
    return ret;
}


ERL_NIF_TERM
erl_chakra_ctx_serialize(ErlChakraRt* rt, ErlChakraJob* job)
{
    ErlChakraScript* script;
    ErlNifBinary source;
    ERL_NIF_TERM ret;
    JsValueRef js_script;
    JsValueRef js_buffer;
    unsigned char* src_buf;
    unsigned char* tgt_buf;
    unsigned int length;
    bool is_disabled;
    JsErrorCode err = JsNoError;

    script = enif_alloc_resource(
            ErlChakraScriptRes,
            sizeof(ErlChakraScript)
        );
    script->env = enif_alloc_env();
    script->attrs = JsParseScriptAttributeNone;
    script->source = enif_make_copy(script->env, job->args[0]);

    if(!enif_inspect_binary(script->env, script->source, &source)) {
        enif_release_resource(script);
        return T2(job->env, ATOM_error, ATOM_bad_script_source);
    }

    err = JsSetCurrentContext(job->ctx->context);
    if(err != JsNoError) {
        goto done;
    }

    err = JsIsRuntimeExecutionDisabled(rt->runtime, &is_disabled);
    if(err != JsNoError) {
        goto done;
    }

    if(is_disabled) {
        err = JsErrorInDisabledState;
        goto done;
    }

    err = JsCreateExternalArrayBuffer(
            source.data,
            source.size,
            NULL,
            NULL,
            &js_script
        );
    if(err != JsNoError) {
        goto done;
    }

    err = JsSerialize(js_script, &js_buffer, script->attrs);
    if(err != JsNoError) {
        goto done;
    }

    err = JsGetArrayBufferStorage(js_buffer, &src_buf, &length);
    if(err != JsNoError) {
        goto done;
    }

    tgt_buf = enif_make_new_binary(script->env, length, &(script->serialized));
    memcpy(tgt_buf, src_buf, length);

    ret = T2(job->env, ATOM_ok, enif_make_resource(job->env, script));

done:
    if(err != JsNoError) {
        js2erl_error(job->env, err, &ret);
    }

    JsSetCurrentContext(JS_INVALID_REFERENCE);
    enif_release_resource(script);
    return ret;
}


ERL_NIF_TERM
erl_chakra_ctx_run(ErlChakraRt* rt, ErlChakraJob* job)
{
    void* res;
    ErlChakraScript* script;

    ErlNifBinary src_url;
    ErlNifBinary serialized;
    ERL_NIF_TERM opt;
    ERL_NIF_TERM opts;
    const ERL_NIF_TERM* tuple;
    int arity;

    JsValueRef js_src_url = JS_INVALID_REFERENCE;
    JsValueRef js_serialized;
    JsValueRef js_result;

    ERL_NIF_TERM ret;
    bool is_disabled;
    JsErrorCode err = JsNoError;

    if(!enif_get_resource(job->env, job->args[0], ErlChakraScriptRes, &res)) {
        return T2(job->env, ATOM_error, ATOM_invalid_argument);
    }
    script = (ErlChakraScript*) res;

    if(!enif_inspect_binary(job->env, script->serialized, &serialized)) {
        return T2(job->env, ATOM_error, ATOM_bad_serialized_script);
    }

    err = JsSetCurrentContext(job->ctx->context);
    if(err != JsNoError) {
        goto done;
    }

    opts = job->args[1];
    if(!enif_is_list(job->env, opts)) {
        ret = T2(job->env, ATOM_error, ATOM_invalid_argument_list);
        goto done;
    }

    err = JsIsRuntimeExecutionDisabled(rt->runtime, &is_disabled);
    if(err != JsNoError) {
        goto done;
    }

    if(is_disabled) {
        err = JsErrorInDisabledState;
        goto done;
    }

    while(enif_get_list_cell(job->env, opts, &opt, &opts)) {
        if(!enif_get_tuple(job->env, opt, &arity, &tuple)) {
            ret = T2(job->env, ATOM_error, ATOM_invalid_argument_list);
            goto done;
        }

        if(arity != 2) {
            ret = T2(job->env, ATOM_error, ATOM_invalid_argument_list);
            goto done;
        }

        if(tuple[0] == ATOM_source_url) {
            if(!enif_inspect_binary(job->env, tuple[1], &src_url)) {
                ret = T2(job->env, ATOM_error, ATOM_invalid_argument);
                goto done;
            }

            err = JsCreateString(
                    (char*) src_url.data,
                    src_url.size,
                    &js_src_url
                );
            if(err != JsNoError) {
                goto done;
            }
        } else {
            ret = T2(job->env, ATOM_error, ATOM_invalid_argument);
            goto done;
        }
    }

    if(js_src_url == JS_INVALID_REFERENCE) {
        err = JsCreateString("<ERLANG>", strlen("<ERLANG>"), &js_src_url);
        if(err != JsNoError) {
            goto done;
        }
    }

    err = JsCreateExternalArrayBuffer(
            serialized.data,
            serialized.size,
            NULL,
            NULL,
            &js_serialized
        );
    if(err != JsNoError) {
        goto done;
    }

    err = JsSetObjectBeforeCollectCallback(
            js_serialized,
            script,
            erl_chakra_collect_serialized_object
        );
    if(err != JsNoError) {
        goto done;
    }

    enif_keep_resource(script);

    err = JsRunSerialized(
            js_serialized,
            erl_chakra_load_script,
            (JsSourceContext) script,
            js_src_url,
            &js_result
        );
    if(err != JsNoError) {
        goto done;
    }

    if(js2erl(job->env, js_result, &ret)) {
        ret = T2(job->env, ATOM_ok, ret);
    }

done:
    if(err != JsNoError) {
        js2erl_error(job->env, err, &ret);
    }

    JsSetCurrentContext(JS_INVALID_REFERENCE);
    return ret;
}


ERL_NIF_TERM
erl_chakra_ctx_call(ErlChakraRt* rt, ErlChakraJob* job)
{
    ERL_NIF_TERM list;
    ERL_NIF_TERM cell;
    ERL_NIF_TERM ret;

    JsValueRef* args = NULL;
    JsValueRef undefined;
    JsValueRef global_obj;
    JsValueRef func;
    JsValueRef result;

    size_t length;
    size_t i;
    bool is_disabled;
    JsErrorCode err = JsNoError;

    err = JsSetCurrentContext(job->ctx->context);
    if(err != JsNoError) {
        goto done;
    }

    err = JsIsRuntimeExecutionDisabled(rt->runtime, &is_disabled);
    if(err != JsNoError) {
        goto done;
    }

    if(is_disabled) {
        err = JsErrorInDisabledState;
        goto done;
    }

    err = JsGetGlobalObject(&global_obj);
    if(err != JsNoError) {
        goto done;
    }

    err = JsGetUndefinedValue(&undefined);
    if(err != JsNoError) {
        goto done;
    }

    ret = erl2js_func(job->env, job->args[0], global_obj, undefined, &func);
    if(ret != ATOM_ok) {
        goto done;
    }

    list = job->args[1];
    if(!enif_is_list(job->env, list)) {
        ret = T2(job->env, ATOM_error, ATOM_invalid_argument_list);
        goto done;
    }

    if(!enif_get_list_length(job->env, list, (unsigned int*) &length)) {
        ret = T2(job->env, ATOM_error, ATOM_invalid_argument_list);
        goto done;
    }

    args = enif_alloc((length + 1) * sizeof(JsValueRef));
    args[0] = global_obj;
    for(i = 1; i <= length; i++) {
        if(!enif_get_list_cell(job->env, list, &cell, &list)) {
            ret = T2(job->env, ATOM_error, ATOM_invalid_argument_list);
            goto done;
        }

        ret = erl2js(job->env, cell, &(args[i]));
        if(ret != ATOM_ok) {
            goto done;
        }
    }

    err = JsCallFunction(func, args, length + 1, &result);
    if(err != JsNoError) {
        goto done;
    }

    if(js2erl(job->env, result, &ret)) {
        ret = T2(job->env, ATOM_ok, ret);
    }

done:
    if(args != NULL) {
        enif_free(args);
    }

    if(err != JsNoError) {
        js2erl_error(job->env, err, &ret);
    }

    JsSetCurrentContext(JS_INVALID_REFERENCE);
    return ret;
}


ERL_NIF_TERM
erl_chakra_ctx_idle(ErlChakraRt* rt, ErlChakraJob* job)
{
    ERL_NIF_TERM ret;
    unsigned int ticks;
    bool is_disabled;
    JsErrorCode err = JsNoError;

    err = JsSetCurrentContext(job->ctx->context);
    if(err != JsNoError) {
        goto done;
    }

    err = JsIsRuntimeExecutionDisabled(rt->runtime, &is_disabled);
    if(err != JsNoError) {
        goto done;
    }

    if(is_disabled) {
        err = JsErrorInDisabledState;
        goto done;
    }

    err = JsIdle(&ticks);
    if(err != JsNoError) {
        goto done;
    }

    ret = T2(job->env, ATOM_ok, enif_make_uint(job->env, ticks));

done:
    if(err != JsNoError) {
        js2erl_error(job->env, err, &ret);
    }

    JsSetCurrentContext(JS_INVALID_REFERENCE);
    return ret;
}


void
erl_chakra_collect_serialized_object(JsValueRef obj, void* script)
{
    enif_release_resource(script);
}


bool
erl_chakra_load_script(
        JsSourceContext src_ctx,
        JsValueRef* value,
        JsParseScriptAttributes* attrs
    )
{
    ErlChakraScript* script = (ErlChakraScript*) src_ctx;
    ErlNifBinary source;
    JsErrorCode err;

    if(!enif_inspect_binary(script->env, script->source, &source)) {
        return false;
    }

    err = JsCreateExternalArrayBuffer(
            source.data,
            source.size,
            NULL,
            NULL,
            value
        );
    if(err != JsNoError) {
        return false;
    }

    *attrs = script->attrs;

    return true;
}


bool
erl_chakra_respond(ErlChakraRt* rt, ErlChakraJob* job, ERL_NIF_TERM msg)
{
    ERL_NIF_TERM wrapped = T2(job->env, job->ref, msg);

    if(!enif_send(NULL, &(rt->pid), job->env, wrapped)) {
        return false;
    }

    return true;
}


void*
rt_loop(void* arg)
{
    ErlChakraRt* rt = (ErlChakraRt*) arg;
    ErlChakraJob* job = NULL;
    ERL_NIF_TERM msg;

    enif_mutex_lock(rt->lock);
    rt->alive = true;
    enif_cond_signal(rt->cond);
    enif_mutex_unlock(rt->lock);

    while(true) {
        enif_mutex_lock(rt->lock);
        while(rt->job == NULL) {
            enif_cond_wait(rt->cond, rt->lock);
        }
        job = rt->job;
        rt->job = NULL;
        enif_cond_signal(rt->cond);
        enif_mutex_unlock(rt->lock);

        switch(job->type) {
            case ERL_CHAKRA_JOB_TYPE_GC:
                msg = erl_chakra_rt_gc(rt, job);
                break;
            case ERL_CHAKRA_JOB_TYPE_ENABLE:
                msg = erl_chakra_rt_enable(rt, job);
                break;
            case ERL_CHAKRA_JOB_TYPE_DISABLE:
                msg = erl_chakra_rt_disable(rt, job);
                break;
            case ERL_CHAKRA_JOB_TYPE_CREATE_CTX:
                msg = erl_chakra_rt_create_context(rt, job);
                break;
            case ERL_CHAKRA_JOB_TYPE_SERIALIZE:
                msg = erl_chakra_ctx_serialize(rt, job);
                break;
            case ERL_CHAKRA_JOB_TYPE_RUN:
                msg = erl_chakra_ctx_run(rt, job);
                break;
            case ERL_CHAKRA_JOB_TYPE_CALL:
                msg = erl_chakra_ctx_call(rt, job);
                break;
            case ERL_CHAKRA_JOB_TYPE_IDLE:
                msg = erl_chakra_ctx_idle(rt, job);
                break;
            default:
                erl_chakra_job_destroy(job);
                enif_mutex_lock(rt->lock);
                rt->alive = false;
                enif_mutex_unlock(rt->lock);
                return NULL;
        }

        erl_chakra_respond(rt, job, msg);
        erl_chakra_job_destroy(job);
    }
}