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
#include "runtime.h"
#include "util.h"


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


static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM num_schedulers)
{
    erl_chakra_init_atoms(env);

    if(!erl_chakra_init_resources(env)) {
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
    const ERL_NIF_TERM* tuple;
    ERL_NIF_TERM opt;
    ERL_NIF_TERM list;
    int memory_limit = -1;
    int stack_size = 128;
    int arity;

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
            } else if(enif_is_identical(tuple[0], ATOM_stack_size)) {
                if(!enif_get_int(env, tuple[1], &stack_size)) {
                    return enif_make_badarg(env);
                }
                if(stack_size < 20 || stack_size > 8192) {
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

    return erl_chakra_runtime_create(env, attrs, memory_limit, stack_size);
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
    ErlChakraJob* job;

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

    job = erl_chakra_job_create();
    job->type = ERL_CHAKRA_JOB_TYPE_GC;

    if(!erl_chakra_runtime_submit(rt, job)) {
        erl_chakra_job_destroy(job);
        return enif_make_badarg(env);
    }

    return T2(env, ATOM_ok, enif_make_copy(env, job->ref));
}


static ERL_NIF_TERM
nif_enable(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraRt* rt;
    ErlChakraJob* job;

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

    job = erl_chakra_job_create();
    job->type = ERL_CHAKRA_JOB_TYPE_ENABLE;

    if(!erl_chakra_runtime_submit(rt, job)) {
        erl_chakra_job_destroy(job);
        return enif_make_badarg(env);
    }

    return T2(env, ATOM_ok, enif_make_copy(env, job->ref));
}


static ERL_NIF_TERM
nif_disable(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraRt* rt;
    ErlChakraJob* job;

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

    job = erl_chakra_job_create();
    job->type = ERL_CHAKRA_JOB_TYPE_DISABLE;

    if(!erl_chakra_runtime_submit(rt, job)) {
        erl_chakra_job_destroy(job);
        return enif_make_badarg(env);
    }

    return T2(env, ATOM_ok, enif_make_copy(env, job->ref));
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
    ErlChakraJob* job;

    if(argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], ErlChakraRtRes, &res_handle)) {
        return enif_make_badarg(env);
    }
    rt = (ErlChakraRt*) res_handle;

    if(!check_pid(env, rt)) {
        return enif_make_badarg(env);
    }

    job = erl_chakra_job_create();
    job->type = ERL_CHAKRA_JOB_TYPE_CREATE_CTX;
    job->args[0] = enif_make_copy(job->env, argv[1]);

    if(!erl_chakra_runtime_submit(rt, job)) {
        erl_chakra_job_destroy(job);
        return enif_make_badarg(env);
    }

    return T2(env, ATOM_ok, enif_make_copy(env, job->ref));
}


static ERL_NIF_TERM
nif_serialize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraCtx* ctx;
    ErlChakraJob* job;

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

    job = erl_chakra_job_create();
    job->type = ERL_CHAKRA_JOB_TYPE_SERIALIZE;
    job->ctx = ctx;
    job->args[0] = enif_make_copy(job->env, argv[1]);

    enif_keep_resource(ctx);

    if(!erl_chakra_runtime_submit(ctx->rt, job)) {
        erl_chakra_job_destroy(job);
        return enif_make_badarg(env);
    }

    return T2(env, ATOM_ok, enif_make_copy(env, job->ref));
}


static ERL_NIF_TERM
nif_run(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraCtx* ctx;
    ErlChakraJob* job;

    if(argc != 3) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], ErlChakraCtxRes, &res_handle)) {
        return enif_make_badarg(env);
    }
    ctx = (ErlChakraCtx*) res_handle;

    if(!check_pid(env, ctx->rt)) {
        return enif_make_badarg(env);
    }

    job = erl_chakra_job_create();
    job->type = ERL_CHAKRA_JOB_TYPE_RUN;
    job->ctx = ctx;
    job->args[0] = enif_make_copy(job->env, argv[1]);
    job->args[1] = enif_make_copy(job->env, argv[2]);

    enif_keep_resource(ctx);

    if(!erl_chakra_runtime_submit(ctx->rt, job)) {
        erl_chakra_job_destroy(job);
        return enif_make_badarg(env);
    }

    return T2(env, ATOM_ok, enif_make_copy(env, job->ref));
}


static ERL_NIF_TERM
nif_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraCtx* ctx;
    ErlChakraJob* job;

    if(argc != 3) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], ErlChakraCtxRes, &res_handle)) {
        return enif_make_badarg(env);
    }
    ctx = (ErlChakraCtx*) res_handle;

    if(!check_pid(env, ctx->rt)) {
        return enif_make_badarg(env);
    }

    job = erl_chakra_job_create();
    job->type = ERL_CHAKRA_JOB_TYPE_CALL;
    job->ctx = ctx;
    job->args[0] = enif_make_copy(job->env, argv[1]);
    job->args[1] = enif_make_copy(job->env, argv[2]);

    enif_keep_resource(ctx);

    if(!erl_chakra_runtime_submit(ctx->rt, job)) {
        erl_chakra_job_destroy(job);
        return enif_make_badarg(env);
    }

    return T2(env, ATOM_ok, enif_make_copy(env, job->ref));
}


static ERL_NIF_TERM
nif_idle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraCtx* ctx;
    ErlChakraJob* job;

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

    job = erl_chakra_job_create();
    job->type = ERL_CHAKRA_JOB_TYPE_IDLE;
    job->ctx = ctx;

    enif_keep_resource(ctx);

    if(!erl_chakra_runtime_submit(ctx->rt, job)) {
        erl_chakra_job_destroy(job);
        return enif_make_badarg(env);
    }

    return T2(env, ATOM_ok, enif_make_copy(env, job->ref));
}


static ERL_NIF_TERM
nif_respond(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraCtx* ctx;
    ErlChakraJob* job;

    if(argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], ErlChakraCtxRes, &res_handle)) {
        return enif_make_badarg(env);
    }
    ctx = (ErlChakraCtx*) res_handle;

    if(!check_pid(env, ctx->rt)) {
        return enif_make_badarg(env);
    }

    job = erl_chakra_job_create();
    job->type = ERL_CHAKRA_JOB_TYPE_RESPONSE;
    job->ctx = ctx;

    enif_keep_resource(ctx);

    if(!erl_chakra_runtime_submit(ctx->rt, job)) {
        erl_chakra_job_destroy(job);
        return enif_make_badarg(env);
    }

    return T2(env, ATOM_ok, enif_make_copy(env, job->ref));
}


static ErlNifFunc funcs[] =
{
    {"nif_create_runtime", 1, nif_create_runtime},
    {"nif_memory_usage", 1, nif_memory_usage},
    {"nif_gc", 1, nif_gc},
    {"nif_enable", 1, nif_enable},
    {"nif_disable", 1, nif_disable},
    {"nif_interrupt", 1, nif_interrupt},

    {"nif_create_context", 2, nif_create_context},
    {"nif_serialize", 2, nif_serialize},
    {"nif_run", 3, nif_run},
    {"nif_call", 3, nif_call},
    {"nif_idle", 1, nif_idle},
    {"nif_respond", 2, nif_respond}
};

ERL_NIF_INIT(chakra, funcs, &load, NULL, NULL, &unload);
