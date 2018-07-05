
#include <assert.h>
#include <math.h>
#include <string.h>
#include <stdio.h>

#include "erl_nif.h"
#include "ChakraCore.h"


#define ERL_CHAKRA_MAX_ATOM_LENGTH 255

#define ERL_CHAKRA_OK 0
#define ERL_CHAKRA_ERROR 1


#define ATOM_MAP(NAME) ERL_NIF_TERM ATOM_##NAME
#include "atoms.h"
#undef ATOM_MAP


ErlNifResourceType* ErlChakraRtRes;
ErlNifResourceType* ErlChakraCtxRes;

typedef struct {
    ErlNifPid pid;
    JsRuntimeHandle runtime;
    bool allow_script_interrupt;
} ErlChakraRt;


typedef struct {
    ErlChakraRt* rt;
    JsContextRef context;
    JsSourceContext source_ctx;
} ErlChakraCtx;


typedef struct {
    ErlNifEnv* env;
    ErlChakraCtx* ctx;

    bool convert_exception;
    bool failed;
} ErlChakraConv;


ERL_NIF_TERM erl2js(ErlChakraConv* conv, ERL_NIF_TERM obj, JsValueRef* out);
ERL_NIF_TERM erl2js_prop_id(
        ErlChakraConv* conv,
        ERL_NIF_TERM obj,
        JsPropertyIdRef* out
    );

ERL_NIF_TERM js2erl(ErlChakraConv* conv, JsValueRef obj);
ERL_NIF_TERM js2erl_error(ErlChakraConv* conv, JsErrorCode err);
ERL_NIF_TERM js2erl_error_code(JsErrorCode err);


ERL_NIF_TERM
t2(ErlNifEnv* env, ERL_NIF_TERM a, ERL_NIF_TERM b)
{
    return enif_make_tuple2(env, a, b);
}


void
init_conv(ErlChakraConv* conv, ErlNifEnv* env)
{
    conv->env = env;
    conv->convert_exception = true;
    conv->failed = false;
}


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
erl_chakra_rt_dtor(ErlNifEnv* env, void* obj)
{
    ErlChakraRt* rt = (ErlChakraRt*) obj;

    if(rt->runtime == JS_INVALID_RUNTIME_HANDLE) {
        return;
    }

    JsDisposeRuntime(rt->runtime);
}


void
erl_chakra_ctx_dtor(ErlNifEnv* env, void* obj)
{
    ErlChakraCtx* ctx = (ErlChakraCtx*) obj;

    if(ctx->rt == NULL) {
        return;
    }

    if(ctx->context != JS_INVALID_REFERENCE) {
        JsRelease(ctx->context, NULL);
    }

    enif_release_resource(ctx->rt);

    return;
}


#define ATOM_MAP(NAME) ATOM_##NAME = enif_make_atom(env, #NAME)
static void
init_atoms(ErlNifEnv* env)
{
    #include "atoms.h"
}
#undef ATOM_MAP


static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    init_atoms(env);

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
    ErlChakraConv conv;
    const ERL_NIF_TERM* tuple;
    ERL_NIF_TERM opt;
    ERL_NIF_TERM list;
    ERL_NIF_TERM ret;
    int memory_limit = -1;
    int arity;
    JsErrorCode err;

    init_conv(&conv, env);
    conv.convert_exception = false;

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
        return js2erl_error(&conv, err);
    }

    err = JsSetRuntimeMemoryLimit(rt->runtime, memory_limit);
    if(err != JsNoError) {
        enif_release_resource(rt);
        return js2erl_error(&conv, err);
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
        return t2(env, ATOM_error, js2erl_error_code(err));
    }

    return t2(env, ATOM_ok, enif_make_uint64(env, memory_usage));
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
        return t2(env, ATOM_error, js2erl_error_code(err));
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
        return t2(env, ATOM_error, js2erl_error_code(err));
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
        return t2(env, ATOM_error, js2erl_error_code(err));
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
        return t2(env, ATOM_error, js2erl_error_code(err));
    }

    return ATOM_ok;
}


static ERL_NIF_TERM
nif_create_context(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraRt* rt = NULL;
    ErlChakraCtx* ctx = NULL;
    ErlChakraConv conv;
    ERL_NIF_TERM ret;
    JsErrorCode err;

    init_conv(&conv, env);
    conv.convert_exception = false;

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
        return js2erl_error(&conv, err);
    }

    err = JsAddRef(ctx->context, NULL);
    if(err != JsNoError) {
        enif_release_resource(ctx);
        return js2erl_error(&conv, err);
    }

    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);

    return enif_make_tuple2(env, ATOM_ok, ret);
}


static ERL_NIF_TERM
nif_run(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraCtx* ctx;
    ErlChakraConv conv;

    ErlNifBinary src;
    ERL_NIF_TERM ret;

    JsValueRef script_name;
    JsValueRef script;
    JsValueRef result;

    JsErrorCode err;

    init_conv(&conv, env);

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

    if(!enif_inspect_binary(env, argv[1], &src)) {
        return enif_make_badarg(env);
    }

    err = JsSetCurrentContext(ctx->context);
    if(err != JsNoError) {
        goto error;
    }

    err = JsCreateString("<ERLANG>", strlen("<ERLANG>"), &script_name);
    if(err != JsNoError) {
        goto error;
    }

    err = JsCreateExternalArrayBuffer(src.data, src.size, NULL, NULL, &script);
    if(err != JsNoError) {
        goto error;
    }

    err = JsRun(
            script,
            ctx->source_ctx++,
            script_name,
            JsParseScriptAttributeNone,
            &result
        );

    if(err != JsNoError) {
        goto error;
    }

    ret = js2erl(&conv, result);
    if(!conv.failed) {
        ret = t2(env, ATOM_ok, ret);
    }


error:
    if(err != JsNoError) {
        ret = js2erl_error(&conv, err);
    }

    JsSetCurrentContext(JS_INVALID_REFERENCE);
    return ret;
}


static ERL_NIF_TERM
nif_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraCtx* ctx;
    ErlChakraConv conv;

    ERL_NIF_TERM list;
    ERL_NIF_TERM cell;
    ERL_NIF_TERM ret;

    JsValueRef* args = NULL;
    JsValueRef undefined;
    JsValueRef global_obj;
    JsValueRef func;
    JsValueRef result;
    JsPropertyIdRef fname;

    bool is_undefined;
    size_t length;
    size_t i;
    JsErrorCode err;

    init_conv(&conv, env);

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

    err = JsSetCurrentContext(ctx->context);
    if(err != JsNoError) {
        return js2erl_error(&conv, err);
    }

    err = erl2js_prop_id(&conv, argv[1], &fname);
    if(err != ATOM_ok) {
        return js2erl_error(&conv, err);
    }

    err = JsGetUndefinedValue(&undefined);
    if(err != JsNoError) {
        return js2erl_error(&conv, err);
    }

    err = JsGetGlobalObject(&global_obj);
    if(err != JsNoError) {
        return js2erl_error(&conv, err);
    }

    err = JsGetProperty(global_obj, fname, &func);
    if(err != JsNoError) {
        return js2erl_error(&conv, err);
    }

    err = JsEquals(func, undefined, &is_undefined);
    if(err != JsNoError) {
        return js2erl_error(&conv, err);
    }

    if(is_undefined) {
        return t2(env, ATOM_error, ATOM_undefined_function);
    }

    list = argv[2];
    if(!enif_is_list(env, list)) {
        return t2(env, ATOM_error, ATOM_invalid_argument_list);
    }

    if(!enif_get_list_length(env, list, (unsigned int*) &length)) {
        return t2(env, ATOM_error, ATOM_invalid_argument_list);
    }

    args = enif_alloc((length + 1) * sizeof(JsValueRef));
    args[0] = global_obj;
    for(i = 1; i <= length; i++) {
        if(!enif_get_list_cell(env, list, &cell, &list)) {
            return t2(env, ATOM_error, ATOM_invalid_argument_list);
        }

        ret = erl2js(&conv, cell, &(args[i]));
        if(ret != ATOM_ok) {
            goto done;
        }
    }

    err = JsCallFunction(func, args, length + 1, &result);
    if(err != JsNoError) {
        goto done;
    }

    ret = js2erl(&conv, result);
    if(!conv.failed) {
        ret = t2(env, ATOM_ok, ret);
    }

done:
    if(args != NULL) {
        enif_free(args);
    }

    if(err != JsNoError) {
        ret = js2erl_error(&conv, err);
    }

    JsSetCurrentContext(JS_INVALID_REFERENCE);
    return ret;
}


static ERL_NIF_TERM
nif_idle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraCtx* ctx;
    ErlChakraConv conv;
    ERL_NIF_TERM ret;
    unsigned int ticks;
    JsErrorCode err;

    init_conv(&conv, env);

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

    err = JsIdle(&ticks);
    if(err != JsNoError) {
        goto done;
    }

    ret = t2(env, ATOM_ok, enif_make_uint(env, ticks));

done:
    if(err != JsNoError) {
        ret = js2erl_error(&conv, err);
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
    {"nif_run", 2, nif_run, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"nif_call", 3, nif_call, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"nif_idle", 1, nif_idle, ERL_NIF_DIRTY_JOB_CPU_BOUND}

};

ERL_NIF_INIT(chakra, funcs, &load, NULL, NULL, &unload);


ERL_NIF_TERM
erl2js_atom(ErlChakraConv* conv, ERL_NIF_TERM obj, JsValueRef* out)
{
    char buf[ERL_CHAKRA_MAX_ATOM_LENGTH];
    size_t size;
    JsErrorCode err;

    if(enif_is_identical(obj, ATOM_undefined)) {
        err = JsGetUndefinedValue(out);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }
        return ATOM_ok;
    }

    if(enif_is_identical(obj, ATOM_null)) {
        err = JsGetNullValue(out);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }
        return ATOM_ok;
    }

    if(enif_is_identical(obj, ATOM_true)) {
        err = JsGetTrueValue(out);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }
        return ATOM_ok;
    }

    if(enif_is_identical(obj, ATOM_false)) {
        err = JsGetFalseValue(out);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }
        return ATOM_ok;
    }

    size = enif_get_atom(conv->env, obj, buf,
            ERL_CHAKRA_MAX_ATOM_LENGTH, ERL_NIF_LATIN1);
    if(size < 1) {
        return t2(conv->env, ATOM_invalid_atom, obj);
    }

    err = JsCreateString(buf, size - 1, out);
    if(err != JsNoError) {
        return js2erl_error(conv, err);
    }

    return ATOM_ok;
}


ERL_NIF_TERM
erl2js_binary(ErlChakraConv* conv, ERL_NIF_TERM obj, JsValueRef* out)
{
    ErlNifBinary bin;
    JsErrorCode err;

    if(!enif_inspect_binary(conv->env, obj, &bin)) {
        return t2(conv->env, ATOM_invalid_binary, obj);
    }

    err = JsCreateString((char*) bin.data, bin.size, out);
    if(err != JsNoError) {
        return js2erl_error(conv, err);
    }

    return ATOM_ok;
}


ERL_NIF_TERM
erl2js_number(ErlChakraConv* conv, ERL_NIF_TERM obj, JsValueRef* out)
{
    int ival;
    double dval;
    JsErrorCode err;

    if(enif_get_int(conv->env, obj, &ival)) {
        err = JsIntToNumber(ival, out);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }

        return ATOM_ok;
    }

    if(enif_get_double(conv->env, obj, &dval)) {
        err = JsDoubleToNumber(dval, out);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }

        return ATOM_ok;
    }

    return t2(conv->env, ATOM_invalid_number, obj);
}


ERL_NIF_TERM
erl2js_list(ErlChakraConv* conv, ERL_NIF_TERM obj, JsValueRef* out)
{
    ErlChakraConv sub_conv;

    ERL_NIF_TERM cell;
    ERL_NIF_TERM term;

    JsValueRef array;
    JsValueRef idx;
    JsValueRef elem;

    unsigned int length;
    int i;
    JsErrorCode err;

    if(!enif_get_list_length(conv->env, obj, &length)) {
        return t2(conv->env, ATOM_invalid_list, obj);
    }

    if(length > (size_t) 2147483647) {
        return ATOM_invalid_list_length;
    }

    err = JsCreateArray(length, &array);
    if(err != JsNoError) {
        return js2erl_error(conv, err);
    }

    for(i = 0; i < length; i++) {
        if(!enif_get_list_cell(conv->env, obj, &cell, &obj)) {
            return t2(conv->env, ATOM_invalid_list, obj);
        }

        init_conv(&sub_conv, conv->env);
        term = erl2js(&sub_conv, cell, &elem);
        if(term != ATOM_ok) {
            return term;
        }

        err = JsIntToNumber(i, &idx);
        if(err != JsNoError) {
            return ATOM_invalid_index;
        }

        err = JsSetIndexedProperty(array, idx, elem);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }
    }

    *out = array;
    return ATOM_ok;
}


ERL_NIF_TERM
erl2js_prop_id(ErlChakraConv* conv, ERL_NIF_TERM obj, JsPropertyIdRef* out)
{
    char buf[ERL_CHAKRA_MAX_ATOM_LENGTH];
    size_t size;
    ErlNifBinary bin;
    JsErrorCode err;

    if(enif_is_atom(conv->env, obj)) {
        size = enif_get_atom(conv->env, obj, buf,
                ERL_CHAKRA_MAX_ATOM_LENGTH, ERL_NIF_LATIN1);
        if(size < 1) {
            return t2(conv->env, ATOM_invalid_key, obj);
        }

        err = JsCreatePropertyId(buf, size, out);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }

        return ATOM_ok;
    }

    if(enif_is_binary(conv->env, obj)) {
        if(!enif_inspect_binary(conv->env, obj, &bin)) {
            return t2(conv->env, ATOM_invalid_key, obj);
        }

        err = JsCreatePropertyId((char*) bin.data, bin.size, out);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }

        return ATOM_ok;
    }

    return t2(conv->env, ATOM_invalid_key, obj);
}


ERL_NIF_TERM
erl2js_object(ErlChakraConv* conv, ERL_NIF_TERM obj, JsValueRef* out)
{
    const ERL_NIF_TERM* items;
    ERL_NIF_TERM props;
    ERL_NIF_TERM cell;
    ERL_NIF_TERM elem;

    JsValueRef ret;
    JsValueRef val;
    JsPropertyIdRef key;

    int arity;
    size_t length;
    size_t i;
    JsErrorCode err;

    if(!enif_get_tuple(conv->env, obj, &arity, &items)) {
        return t2(conv->env, ATOM_invalid_term, obj);
    }

    if(arity != 1) {
        return t2(conv->env, ATOM_invalid_term, obj);
    }

    props = items[0];

    if(!enif_is_list(conv->env, props)) {
        return t2(conv->env, ATOM_invalid_term, obj);
    }

    if(!enif_get_list_length(conv->env, props, (unsigned int*) &length)) {
        return t2(conv->env, ATOM_invalid_term, obj);
    }

    err = JsCreateObject(&ret);
    if(err != JsNoError) {
        return js2erl_error(conv, err);
    }

    for(i = 0; i < length; i++) {
        if(!enif_get_list_cell(conv->env, props, &cell, &props)) {
            return t2(conv->env, ATOM_invalid_props, props);
        }

        if(!enif_is_tuple(conv->env, cell)) {
            return t2(conv->env, ATOM_invalid_property, cell);
        }

        if(!enif_get_tuple(conv->env, cell, &arity, &items)) {
            return t2(conv->env, ATOM_invalid_property, cell);
        }

        if(arity != 2) {
            return t2(conv->env, ATOM_invalid_property, cell);
        }

        elem = erl2js_prop_id(conv, items[0], &key);
        if(elem != ATOM_ok) {
            return elem;
        }

        elem = erl2js(conv, items[1], &val);
        if(elem != ATOM_ok) {
            return elem;
        }

        // TODO: Strict rules?
        err = JsSetProperty(ret, key, val, false);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }
    }

    *out = ret;
    return ATOM_ok;
}


ERL_NIF_TERM
erl2js(ErlChakraConv* conv, ERL_NIF_TERM obj, JsValueRef* out)
{
    if(enif_is_atom(conv->env, obj)) {
        return erl2js_atom(conv, obj, out);
    }

    if(enif_is_binary(conv->env, obj)) {
        return erl2js_binary(conv, obj, out);
    }

    if(enif_is_number(conv->env, obj)) {
        return erl2js_number(conv, obj, out);
    }

    if(enif_is_list(conv->env, obj)) {
        return erl2js_list(conv, obj, out);
    }

    if(enif_is_tuple(conv->env, obj)) {
        return erl2js_object(conv, obj, out);
    }

    return t2(conv->env, ATOM_invalid_term, obj);
}


ERL_NIF_TERM
js2erl_number(ErlChakraConv* conv, JsValueRef obj)
{
    double dval;
    ErlNifSInt64 ival;
    JsErrorCode err;

    err = JsNumberToDouble(obj, &dval);
    if(err != JsNoError) {
        return js2erl_error(conv, err);
    }

    if(dval == floor(dval) && !isinf(dval)) {
        ival = (ErlNifSInt64) dval;
        return enif_make_int64(conv->env, ival);
    } else {
        return enif_make_double(conv->env, dval);
    }
}


ERL_NIF_TERM
js2erl_string(ErlChakraConv* conv, JsValueRef obj)
{
    ERL_NIF_TERM result;
    unsigned char* buf;
    size_t length;
    JsErrorCode err;

    err = JsCopyString(obj, NULL, 0, &length);
    if(err != JsNoError) {
        return js2erl_error(conv, err);
    }

    buf = enif_make_new_binary(conv->env, length, &result);
    err = JsCopyString(obj, (char*) buf, length, NULL);
    if(err != JsNoError) {
        return js2erl_error(conv, err);
    }

    return result;
}


ERL_NIF_TERM
js2erl_bool(ErlChakraConv* conv, JsValueRef obj)
{
    bool val;
    JsErrorCode err;

    err = JsBooleanToBool(obj, &val);
    if(err != JsNoError) {
        return js2erl_error(conv, err);
    }

    if(val) {
        return ATOM_true;
    } else {
        return ATOM_false;
    }
}


JsErrorCode
js_to_prop_id(JsValueRef obj, JsPropertyIdRef* out)
{
    char* buf;
    size_t length;
    JsErrorCode err;

    err = JsCopyString(obj, NULL, 0, &length);
    if(err != JsNoError) {
        return err;
    }

    buf = enif_alloc(length * sizeof(char));
    err = JsCopyString(obj, buf, length, NULL);
    if(err != JsNoError) {
        enif_free(buf);
        return err;
    }

    err = JsCreatePropertyId(buf, length, out);
    if(err != JsNoError) {
        enif_free(buf);
        return err;
    }

    enif_free(buf);
    return JsNoError;
}


ERL_NIF_TERM
js2erl_object(ErlChakraConv* conv, JsValueRef obj)
{
    ErlChakraConv sub_conv;

    ERL_NIF_TERM props = enif_make_list(conv->env, 0);
    ERL_NIF_TERM erl_key;
    ERL_NIF_TERM erl_val;
    ERL_NIF_TERM pair;

    JsValueRef prop_names;
    JsPropertyIdRef prop;
    JsValueRef idx;
    JsValueRef js_key;
    JsValueRef js_val;

    int length;
    int i;
    JsErrorCode err;

    err = JsGetOwnPropertyNames(obj, &prop_names);
    // Kinda hack, but if we hit out of memory then
    // this call fails with out of memory as well...
    if(err == JsErrorScriptException) {
        return ATOM_out_of_memory;
    }
    if(err != JsNoError) {
        return js2erl_error(conv, err);
    }

    err = JsCreatePropertyId("length", strlen("length"), &prop);
    if(err != JsNoError) {
        return js2erl_error(conv, err);
    }

    err = JsGetProperty(prop_names, prop, &js_val);
    if(err != JsNoError) {
        return js2erl_error(conv, err);
    }

    err = JsNumberToInt(js_val, &length);
    if(err != JsNoError) {
        return js2erl_error(conv, err);
    }

    for(i = length - 1; i >= 0; i--) {
        err = JsIntToNumber(i, &idx);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }

        err = JsGetIndexedProperty(prop_names, idx, &js_key);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }

        err = js_to_prop_id(js_key, &prop);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }

        err = JsGetProperty(obj, prop, &js_val);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }

        init_conv(&sub_conv, conv->env);
        erl_key = js2erl(&sub_conv, js_key);
        if(sub_conv.failed) {
            return erl_key;
        }

        init_conv(&sub_conv, conv->env);
        erl_val = js2erl(&sub_conv, js_val);
        if(sub_conv.failed) {
            return erl_val;
        }

        pair = t2(conv->env, erl_key, erl_val);
        props = enif_make_list_cell(conv->env, pair, props);
    }

    return enif_make_tuple1(conv->env, props);
}


ERL_NIF_TERM
js2erl_error_object(ErlChakraConv* conv, JsValueRef obj)
{
    return js2erl_object(conv, obj);
}


ERL_NIF_TERM
js2erl_list(ErlChakraConv* conv, JsValueRef obj)
{
    ErlChakraConv sub_conv;

    ERL_NIF_TERM vals = enif_make_list(conv->env, 0);
    ERL_NIF_TERM erl_val;

    JsPropertyIdRef prop;
    JsValueRef idx;
    JsValueRef js_val;

    int length;
    int i;
    JsErrorCode err;

    err = JsCreatePropertyId("length", strlen("length"), &prop);
    if(err != JsNoError) {
        return js2erl_error(conv, err);
    }

    err = JsGetProperty(obj, prop, &js_val);
    if(err != JsNoError) {
        return js2erl_error(conv, err);
    }

    err = JsNumberToInt(js_val, &length);
    if(err != JsNoError) {
        return js2erl_error(conv, err);
    }

    for(i = length - 1; i >= 0; i--) {
        err = JsIntToNumber(i, &idx);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }

        err = JsGetIndexedProperty(obj, idx, &js_val);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }

        init_conv(&sub_conv, conv->env);
        erl_val = js2erl(&sub_conv, js_val);
        if(sub_conv.failed) {
            return erl_val;
        }

        vals = enif_make_list_cell(conv->env, erl_val, vals);
    }

    return vals;
}


ERL_NIF_TERM
js2erl_any(ErlChakraConv* conv, JsValueRef obj)
{
    ERL_NIF_TERM result;

    JsValueRef str;

    unsigned char* buf;
    size_t length;
    JsErrorCode err;

    err = JsConvertValueToString(obj, &str);
    if(err != JsNoError) {
        return js2erl_error(conv, err);
    }

    err = JsCopyString(str, NULL, 0, &length);
    if(err != JsNoError) {
        return js2erl_error(conv, err);
    }

    buf = enif_make_new_binary(conv->env, length, &result);
    err = JsCopyString(str, (char*) buf, length, NULL);
    if(err != JsNoError) {
        return js2erl_error(conv, err);
    }

    return result;
}


ERL_NIF_TERM
js2erl(ErlChakraConv* conv, JsValueRef obj)
{
    JsValueType vt;
    JsErrorCode err;

    err = JsGetValueType(obj, &vt);
    if(err != JsNoError) {
        return js2erl_error(conv, err);
    }

    switch(vt) {
        case JsUndefined:
            return ATOM_undefined;
        case JsNull:
            return ATOM_null;
        case JsNumber:
            return js2erl_number(conv, obj);
        case JsString:
            return js2erl_string(conv, obj);
        case JsBoolean:
            return js2erl_bool(conv, obj);
        case JsObject:
            return js2erl_object(conv, obj);
        case JsError:
            return js2erl_error_object(conv, obj);
        case JsArray:
            return js2erl_list(conv, obj);
        case JsSymbol:
        case JsFunction:
        case JsArrayBuffer:
        case JsTypedArray:
        case JsDataView:
            return js2erl_any(conv, obj);
        default:
            return t2(conv->env, ATOM_error, ATOM_invalid_value_type);
    }
}


ERL_NIF_TERM
js2erl_error(ErlChakraConv* conv, JsErrorCode err)
{
    ERL_NIF_TERM ret;

    JsValueRef exc;
    JsErrorCode sub_err;

    bool has_exc;

    if(!conv->convert_exception) {
        conv->failed = true;
        return t2(conv->env, ATOM_error, js2erl_error_code(err));
    }

    // Prevent infinite recursion in case we generate
    // an exception when converting an exception.
    conv->convert_exception = false;

    sub_err = JsHasException(&has_exc);
    if(sub_err != JsNoError) {
        return js2erl_error(conv, sub_err);
    }

    if(has_exc || err == JsErrorScriptException) {
        sub_err = JsGetAndClearException(&exc);
        if(sub_err != JsNoError) {
            return js2erl_error(conv, sub_err);
        }

        ret = js2erl(conv, exc);
        if(!conv->failed) {
            ret = t2(conv->env, ATOM_exception, ret);
        }
    } else {
        ret = t2(conv->env, ATOM_error, js2erl_error_code(err));
    }

    conv->failed = true;
    return ret;
}


#define ERROR_MAP(ERROR, NAME) if(err == ERROR) return ATOM_##NAME
ERL_NIF_TERM
js2erl_error_code(JsErrorCode err)
{
    #include "errors.h"
    return ATOM_unknown;
}
#undef ERROR_MAP
