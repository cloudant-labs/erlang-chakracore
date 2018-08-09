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

#include <assert.h>
#include <string.h>
#include <stdio.h>

#include "erl_nif.h"
#include "ChakraCore.h"

#include "atoms.h"
#include "erl2js.h"
#include "js2erl.h"
#include "resources.h"
#include "util.h"


ErlNifResourceType* ErlChakraRtRes;
ErlNifResourceType* ErlChakraCtxRes;
ErlNifResourceType* ErlChakraSerializedScriptRes;



bool
check_pid(ErlNifEnv* env, ErlChakraRt* rt)
{
    ErlNifPid pid;
    enif_self(env, &pid);

    if(enif_compare(pid.pid, rt->pid.pid) == 0) {
        return true;
    }

    return false;
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
    ErlChakraSerializedScript* script = (ErlChakraSerializedScript*) src_ctx;
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


static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM num_schedulers)
{
    erl_chakra_init_atoms(env);

    ErlChakraRtRes = enif_open_resource_type(
            env,
            NULL,
            "erl_chakra_runtime",
            erl_chakra_rt_dtor,
            ERL_NIF_RT_CREATE,
            NULL
        );
    if(ErlChakraRtRes == NULL) {
        return 1;
    }

    ErlChakraCtxRes = enif_open_resource_type(
            env,
            NULL,
            "erl_chakra_context",
            erl_chakra_ctx_dtor,
            ERL_NIF_RT_CREATE,
            NULL
        );
    if(ErlChakraCtxRes == NULL) {
        return 1;
    }

    ErlChakraSerializedScriptRes = enif_open_resource_type(
            env,
            NULL,
            "erl_chakra_serialized_script",
            erl_chakra_serialized_script_dtor,
            ERL_NIF_RT_CREATE,
            NULL
        );
    if(ErlChakraSerializedScriptRes == NULL) {
        return 1;
    }

    return 0;
}


static void
unload(ErlNifEnv* env, void* priv)
{
    return;
}


static ERL_NIF_TERM
nif_create_runtime(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlChakraRt* rt = NULL;
    const ERL_NIF_TERM* tuple;
    ERL_NIF_TERM opt;
    ERL_NIF_TERM list;
    ERL_NIF_TERM ret;
    int memory_limit = -1;
    int arity;
    JsErrorCode err;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_is_list(env, argv[0])) {
        return enif_make_badarg(env);
    }

    list = argv[0];
    JsRuntimeAttributes attrs = JsRuntimeAttributeNone;
    while(enif_get_list_cell(env, list, &opt, &list)) {
        if(enif_is_tuple(env, opt)) {
            if(!enif_get_tuple(env, opt, &arity, &tuple)) {
                return enif_make_badarg(env);
            }
            if(arity != 2) {
                return enif_make_badarg(env);
            }
            if(enif_is_identical(tuple[0], ATOM_memory_limit)) {
                if(!enif_get_int(env, tuple[1], &memory_limit)) {
                    return enif_make_badarg(env);
                }
                if(memory_limit < -1) {
                    return enif_make_badarg(env);
                }
            } else {
                return enif_make_badarg(env);
            }
        } else if(enif_is_identical(opt, ATOM_disable_background_work)) {
            attrs |= JsRuntimeAttributeDisableBackgroundWork;
        } else if(enif_is_identical(opt, ATOM_allow_script_interrupt)) {
            attrs |= JsRuntimeAttributeAllowScriptInterrupt;
        } else if(enif_is_identical(opt, ATOM_enable_idle_processing)) {
            attrs |= JsRuntimeAttributeEnableIdleProcessing;
        } else if(enif_is_identical(opt, ATOM_disable_native_code_generation)) {
            attrs |= JsRuntimeAttributeDisableNativeCodeGeneration;
        } else if(enif_is_identical(opt, ATOM_disable_eval)) {
            attrs |= JsRuntimeAttributeDisableEval;
        } else if(enif_is_identical(opt, ATOM_enable_experimental_features)) {
            attrs |= JsRuntimeAttributeEnableExperimentalFeatures;
        } else {
            return enif_make_badarg(env);
        }
    }

    rt = enif_alloc_resource(ErlChakraRtRes, sizeof(ErlChakraRt));
    rt->runtime = JS_INVALID_RUNTIME_HANDLE;

    if((attrs & JsRuntimeAttributeAllowScriptInterrupt) == 0) {
        rt->allow_script_interrupt = false;
    } else {
        rt->allow_script_interrupt = true;
    }

    err = JsCreateRuntime(attrs, NULL, &(rt->runtime));
    if(err != JsNoError) {
        rt->runtime = JS_INVALID_RUNTIME_HANDLE;
        enif_release_resource(rt);
        js2erl_error(env, err, &ret);
        return ret;
    }

    err = JsSetRuntimeMemoryLimit(rt->runtime, memory_limit);
    if(err != JsNoError) {
        enif_release_resource(rt);
        js2erl_error(env, err, &ret);
        return ret;
    }

    enif_self(env, &(rt->pid));
    ret = enif_make_resource(env, rt);
    enif_release_resource(rt);

    return enif_make_tuple2(env, ATOM_ok, ret);

}


static ERL_NIF_TERM
nif_memory_usage(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraRt* rt;
    size_t memory_usage;
    JsErrorCode err;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], ErlChakraRtRes, &res_handle)) {
        return enif_make_badarg(env);
    }
    rt = (ErlChakraRt*) res_handle;

    // Memory usage can be retrieved from any thread

    err = JsGetRuntimeMemoryUsage(rt->runtime, &memory_usage);
    if(err != JsNoError) {
        return T2(env, ATOM_error, js2erl_error_code(err));
    }

    return T2(env, ATOM_ok, enif_make_uint64(env, memory_usage));
}


static ERL_NIF_TERM
nif_gc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraRt* rt;
    JsErrorCode err;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], ErlChakraRtRes, &res_handle)) {
        return enif_make_badarg(env);
    }
    rt = (ErlChakraRt*) res_handle;

    if(!check_pid(env, rt)) {
        return enif_make_badarg(env);
    }

    err = JsCollectGarbage(rt->runtime);
    if(err != JsNoError) {
        return T2(env, ATOM_error, js2erl_error_code(err));
    }

    return ATOM_ok;
}


static ERL_NIF_TERM
nif_enable(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraRt* rt;
    JsErrorCode err;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], ErlChakraRtRes, &res_handle)) {
        return enif_make_badarg(env);
    }
    rt = (ErlChakraRt*) res_handle;

    if(!check_pid(env, rt)) {
        return enif_make_badarg(env);
    }

    err = JsEnableRuntimeExecution(rt->runtime);
    if(err != JsNoError) {
        return T2(env, ATOM_error, js2erl_error_code(err));
    }

    return ATOM_ok;
}


static ERL_NIF_TERM
nif_disable(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraRt* rt;
    JsErrorCode err;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], ErlChakraRtRes, &res_handle)) {
        return enif_make_badarg(env);
    }
    rt = (ErlChakraRt*) res_handle;

    if(!check_pid(env, rt)) {
        return enif_make_badarg(env);
    }

    err = JsDisableRuntimeExecution(rt->runtime);
    if(err != JsNoError) {
        return T2(env, ATOM_error, js2erl_error_code(err));
    }

    return ATOM_ok;
}


static ERL_NIF_TERM
nif_interrupt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraRt* rt;
    JsErrorCode err;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], ErlChakraRtRes, &res_handle)) {
        return enif_make_badarg(env);
    }
    rt = (ErlChakraRt*) res_handle;

    // No check pid as interrupts can come from different
    // threads and Chakra guarantees that this is kosher.

    // JSRT behaves kinda funny returning no_current_context
    // if the Runtime was created without AllowScriptInterrupt
    // so we have to check our stored flag.
    if(!rt->allow_script_interrupt) {
        return enif_make_badarg(env);
    }

    err = JsDisableRuntimeExecution(rt->runtime);
    if(err != JsNoError) {
        return T2(env, ATOM_error, js2erl_error_code(err));
    }

    return ATOM_ok;
}


static ERL_NIF_TERM
nif_create_context(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraRt* rt = NULL;
    ErlChakraCtx* ctx = NULL;
    ERL_NIF_TERM ret;
    JsErrorCode err;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], ErlChakraRtRes, &res_handle)) {
        return enif_make_badarg(env);
    }
    rt = (ErlChakraRt*) res_handle;

    ctx = enif_alloc_resource(ErlChakraCtxRes, sizeof(ErlChakraCtx));
    ctx->rt = rt;
    ctx->context = JS_INVALID_REFERENCE;

    enif_keep_resource(ctx->rt);

    err = JsCreateContext(ctx->rt->runtime, &(ctx->context));
    if(err != JsNoError) {
        ctx->context = JS_INVALID_REFERENCE;
        enif_release_resource(ctx);
        js2erl_error(env, err, &ret);
        return ret;
    }

    err = JsAddRef(ctx->context, NULL);
    if(err != JsNoError) {
        enif_release_resource(ctx);
        js2erl_error(env, err, &ret);
        return ret;
    }

    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);

    return enif_make_tuple2(env, ATOM_ok, ret);
}


static ERL_NIF_TERM
nif_serialize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraCtx* ctx;
    ErlChakraSerializedScript* script;
    ErlNifBinary source;
    ERL_NIF_TERM ret;
    JsValueRef js_script;
    JsValueRef js_buffer;
    unsigned char* src_buf;
    unsigned char* tgt_buf;
    unsigned int length;
    bool is_disabled;
    JsErrorCode err = JsNoError;

    if(argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], ErlChakraCtxRes, &res_handle)) {
        return enif_make_badarg(env);
    }
    ctx = (ErlChakraCtx*) res_handle;

    if(!enif_is_binary(env, argv[1])) {
        return enif_make_badarg(env);
    }

    if(!check_pid(env, ctx->rt)) {
        return enif_make_badarg(env);
    }

    script = enif_alloc_resource(
            ErlChakraSerializedScriptRes,
            sizeof(ErlChakraSerializedScript)
        );
    script->env = enif_alloc_env();
    script->attrs = JsParseScriptAttributeNone;
    script->source = enif_make_copy(script->env, argv[1]);

    if(!enif_inspect_binary(script->env, script->source, &source)) {
        enif_release_resource(script);
        return enif_make_badarg(env);
    }

    err = JsSetCurrentContext(ctx->context);
    if(err != JsNoError) {
        goto done;
    }

    err = JsIsRuntimeExecutionDisabled(ctx->rt->runtime, &is_disabled);
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

    ret = T2(env, ATOM_ok, enif_make_resource(env, script));

done:
    if(err != JsNoError) {
        js2erl_error(env, err, &ret);
    }

    JsSetCurrentContext(JS_INVALID_REFERENCE);
    enif_release_resource(script);
    return ret;
}


static ERL_NIF_TERM
nif_run(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraCtx* ctx;
    ErlChakraSerializedScript* script;

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

    if(argc != 3) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], ErlChakraCtxRes, &res_handle)) {
        return enif_make_badarg(env);
    }
    ctx = (ErlChakraCtx*) res_handle;

    if(!enif_get_resource(
            env, argv[1], ErlChakraSerializedScriptRes, &res_handle)) {
        return enif_make_badarg(env);
    }
    script = (ErlChakraSerializedScript*) res_handle;

    if(!check_pid(env, ctx->rt)) {
        return enif_make_badarg(env);
    }

    if(!enif_inspect_binary(script->env, script->serialized, &serialized)) {
        return enif_make_badarg(env);
    }

    err = JsSetCurrentContext(ctx->context);
    if(err != JsNoError) {
        goto done;
    }

    opts = argv[2];
    if(!enif_is_list(env, opts)) {
        ret = enif_make_badarg(env);
        goto done;
    }

    err = JsIsRuntimeExecutionDisabled(ctx->rt->runtime, &is_disabled);
    if(err != JsNoError) {
        goto done;
    }

    if(is_disabled) {
        err = JsErrorInDisabledState;
        goto done;
    }

    while(enif_get_list_cell(env, opts, &opt, &opts)) {
        if(!enif_get_tuple(env, opt, &arity, &tuple)) {
            ret = enif_make_badarg(env);
            goto done;
        }

        if(arity != 2) {
            ret = enif_make_badarg(env);
            goto done;
        }

        if(tuple[0] == ATOM_source_url) {
            if(!enif_inspect_binary(env, tuple[1], &src_url)) {
                ret = enif_make_badarg(env);
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
            ret = enif_make_badarg(env);
            goto done;
        }
    }

    if(is_disabled) {
        err = JsErrorInDisabledState;
        goto done;
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

    if(js2erl(env, js_result, &ret)) {
        ret = T2(env, ATOM_ok, ret);
    }

done:
    if(err != JsNoError) {
        js2erl_error(env, err, &ret);
    }

    JsSetCurrentContext(JS_INVALID_REFERENCE);
    return ret;
}


static ERL_NIF_TERM
nif_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraCtx* ctx;

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

    if(argc != 3) {
        ret = enif_make_badarg(env);
        goto done;
    }

    if(!enif_get_resource(env, argv[0], ErlChakraCtxRes, &res_handle)) {
        ret = enif_make_badarg(env);
        goto done;
    }
    ctx = (ErlChakraCtx*) res_handle;

    if(!check_pid(env, ctx->rt)) {
        ret = enif_make_badarg(env);
        goto done;
    }

    err = JsSetCurrentContext(ctx->context);
    if(err != JsNoError) {
        goto done;
    }

    err = JsIsRuntimeExecutionDisabled(ctx->rt->runtime, &is_disabled);
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

    ret = erl2js_func(env, argv[1], global_obj, undefined, &func);
    if(ret != ATOM_ok) {
        goto done;
    }

    list = argv[2];
    if(!enif_is_list(env, list)) {
        ret = T2(env, ATOM_error, ATOM_invalid_argument_list);
        goto done;
    }

    if(!enif_get_list_length(env, list, (unsigned int*) &length)) {
        ret = T2(env, ATOM_error, ATOM_invalid_argument_list);
        goto done;
    }

    args = enif_alloc((length + 1) * sizeof(JsValueRef));
    args[0] = global_obj;
    for(i = 1; i <= length; i++) {
        if(!enif_get_list_cell(env, list, &cell, &list)) {
            ret = T2(env, ATOM_error, ATOM_invalid_argument_list);
            goto done;
        }

        ret = erl2js(env, cell, &(args[i]));
        if(ret != ATOM_ok) {
            goto done;
        }
    }

    err = JsCallFunction(func, args, length + 1, &result);
    if(err != JsNoError) {
        goto done;
    }

    if(js2erl(env, result, &ret)) {
        ret = T2(env, ATOM_ok, ret);
    }

done:
    if(args != NULL) {
        enif_free(args);
    }

    if(err != JsNoError) {
        js2erl_error(env, err, &ret);
    }

    JsSetCurrentContext(JS_INVALID_REFERENCE);
    return ret;
}


static ERL_NIF_TERM
nif_idle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraCtx* ctx;
    ERL_NIF_TERM ret;
    unsigned int ticks;
    bool is_disabled;
    JsErrorCode err = JsNoError;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], ErlChakraCtxRes, &res_handle)) {
        return enif_make_badarg(env);
    }
    ctx = (ErlChakraCtx*) res_handle;

    if(!check_pid(env, ctx->rt)) {
        return enif_make_badarg(env);
    }

    err = JsSetCurrentContext(ctx->context);
    if(err != JsNoError) {
        goto done;
    }

    err = JsIsRuntimeExecutionDisabled(ctx->rt->runtime, &is_disabled);
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

    ret = T2(env, ATOM_ok, enif_make_uint(env, ticks));

done:
    if(err != JsNoError) {
        js2erl_error(env, err, &ret);
    }

    JsSetCurrentContext(JS_INVALID_REFERENCE);
    return ret;
}


static ErlNifFunc funcs[] =
{
    {"nif_create_runtime", 1, nif_create_runtime, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"nif_memory_usage", 1, nif_memory_usage, 0},
    {"nif_gc", 1, nif_gc, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"nif_enable", 1, nif_enable, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"nif_disable", 1, nif_disable, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"nif_interrupt", 1, nif_interrupt, 0},

    {"nif_create_context", 1, nif_create_context, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"nif_serialize", 2, nif_serialize, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"nif_run", 3, nif_run, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"nif_call", 3, nif_call, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"nif_idle", 1, nif_idle, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

ERL_NIF_INIT(chakra, funcs, &load, NULL, NULL, &unload);


