
#include <assert.h>
#include <math.h>
#include <string.h>
#include <stdio.h>

#include "erl_nif.h"
#include "ChakraCore.h"


#define ERL_CHAKRA_MAX_ATOM_LENGTH 255

#define ERL_CHAKRA_OK 0
#define ERL_CHAKRA_ERROR 1


ERL_NIF_TERM atom_ok;
ERL_NIF_TERM atom_error;
ERL_NIF_TERM atom_exception;
ERL_NIF_TERM atom_false;
ERL_NIF_TERM atom_invalid_term;
ERL_NIF_TERM atom_null;
ERL_NIF_TERM atom_true;
ERL_NIF_TERM atom_undefined;
ERL_NIF_TERM atom_unknown;

// Runtime Attribute atoms
ERL_NIF_TERM atom_disable_background_work;
ERL_NIF_TERM atom_allow_script_interrupt;
ERL_NIF_TERM atom_enable_idle_processing;
ERL_NIF_TERM atom_disable_native_code_generation;
ERL_NIF_TERM atom_disable_eval;
ERL_NIF_TERM atom_enable_experimental_features;
// Ignoring dispatch set exception

ErlNifResourceType* ErlChakraCtxRes;

typedef struct {
    ErlNifPid pid;
    JsRuntimeHandle runtime;
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
ERL_NIF_TERM js2erl_error_code(ErlChakraConv* conv, JsErrorCode err);

ERL_NIF_TERM
t2(ErlNifEnv* env, ERL_NIF_TERM a, ERL_NIF_TERM b)
{
    return enif_make_tuple2(env, a, b);
}


ERL_NIF_TERM
mkatom(ErlNifEnv* env, const char* name)
{
    return enif_make_atom(env, name);
}


void
init_conv(ErlChakraConv* conv, ErlNifEnv* env)
{
    conv->env = env;
    conv->convert_exception = true;
    conv->failed = false;
}


bool
check_pid(ErlNifEnv* env, ErlChakraCtx* ctx)
{
    ErlNifPid pid;
    enif_self(env, &pid);

    if(enif_compare(pid.pid, ctx->pid.pid) == 0) {
        return true;
    }

    return false;
}


void
erl_chakra_ctx_dtor(ErlNifEnv* env, void* obj)
{
    ErlChakraCtx* ctx = (ErlChakraCtx*) obj;

    if(ctx->runtime != JS_INVALID_RUNTIME_HANDLE) {
        JsSetCurrentContext(JS_INVALID_REFERENCE);
        JsDisposeRuntime(ctx->runtime);
    }

    return;
}


static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    atom_ok = mkatom(env, "ok");
    atom_error = mkatom(env, "error");
    atom_exception = mkatom(env, "exception");
    atom_false = mkatom(env, "false");
    atom_invalid_term = mkatom(env, "invalid_term");
    atom_null = mkatom(env, "null");
    atom_true = mkatom(env, "true");
    atom_undefined = mkatom(env, "undefined");
    atom_unknown = mkatom(env, "unknown");

    atom_disable_background_work = mkatom(env, "disable_background_work");
    atom_allow_script_interrupt = mkatom(env, "allow_script_interrupt");
    atom_enable_idle_processing = mkatom(env, "enable_idle_processing");
    atom_disable_native_code_generation =
            mkatom(env, "disable_native_code_generation");
    atom_disable_eval = mkatom(env, "disable_eval");
    atom_enable_experimental_features =
            mkatom(env, "enable_experimental_features");

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
nif_create_context(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlChakraCtx* ctx = NULL;
    ErlChakraConv conv;
    ERL_NIF_TERM opt;
    ERL_NIF_TERM list;
    ERL_NIF_TERM ret;
    JsErrorCode err;

    init_conv(&conv, env);

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_is_list(env, argv[0])) {
        return enif_make_badarg(env);
    }

    list = argv[0];
    JsRuntimeAttributes attrs = JsRuntimeAttributeNone;
    while(enif_get_list_cell(env, list, &opt, &list)) {
        if(enif_is_identical(opt, atom_disable_background_work)) {
            attrs |= JsRuntimeAttributeDisableBackgroundWork;
        } else if(enif_is_identical(opt, atom_allow_script_interrupt)) {
            attrs |= JsRuntimeAttributeAllowScriptInterrupt;
        } else if(enif_is_identical(opt, atom_enable_idle_processing)) {
            attrs |= JsRuntimeAttributeEnableIdleProcessing;
        } else if(enif_is_identical(opt, atom_disable_native_code_generation)) {
            attrs |= JsRuntimeAttributeDisableNativeCodeGeneration;
        } else if(enif_is_identical(opt, atom_disable_eval)) {
            attrs |= JsRuntimeAttributeDisableEval;
        } else if(enif_is_identical(opt, atom_enable_experimental_features)) {
            attrs |= JsRuntimeAttributeEnableExperimentalFeatures;
        } else {
            return enif_make_badarg(env);
        }
    }

    ctx = enif_alloc_resource(ErlChakraCtxRes, sizeof(ErlChakraCtx));
    ctx->runtime = JS_INVALID_RUNTIME_HANDLE;

    err = JsCreateRuntime(attrs, NULL, &(ctx->runtime));
    if(err != JsNoError) {
        ctx->runtime = JS_INVALID_RUNTIME_HANDLE;
        enif_release_resource(ctx);
        return js2erl_error(&conv, err);
    }

    err = JsCreateContext(ctx->runtime, &(ctx->context));
    if(err != JsNoError) {
        enif_release_resource(ctx);
        return js2erl_error(&conv, err);
    }

    err = JsAddRef(ctx->context, NULL);
    if(err != JsNoError) {
        enif_release_resource(ctx);
        return js2erl_error(&conv, err);
    }

    enif_self(env, &(ctx->pid));
    ctx->source_ctx = 0;

    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);

    return enif_make_tuple2(env, atom_ok, ret);
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

    if(!check_pid(env, ctx)) {
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
        ret = t2(env, atom_ok, ret);
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

    if(!check_pid(env, ctx)) {
        return enif_make_badarg(env);
    }

    err = JsSetCurrentContext(ctx->context);
    if(err != JsNoError) {
        return js2erl_error(&conv, err);
    }

    err = erl2js_prop_id(&conv, argv[1], &fname);
    if(err != atom_ok) {
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
        return t2(env, atom_error, mkatom(env, "undefined_function"));
    }

    list = argv[2];
    if(!enif_is_list(env, list)) {
        return t2(env, atom_error, mkatom(env, "invalid_argument_list"));
    }

    if(!enif_get_list_length(env, list, (unsigned int*) &length)) {
        return t2(env, atom_error, mkatom(env, "invalid_argument_list"));
    }

    args = enif_alloc((length + 1) * sizeof(JsValueRef));
    args[0] = global_obj;
    for(i = 1; i <= length; i++) {
        if(!enif_get_list_cell(env, list, &cell, &list)) {
            return t2(env, atom_error, mkatom(env, "invalid_argument_list"));
        }

        ret = erl2js(&conv, cell, &(args[i]));
        if(ret != atom_ok) {
            goto done;
        }
    }

    err = JsCallFunction(func, args, length + 1, &result);
    if(err != JsNoError) {
        goto done;
    }

    ret = js2erl(&conv, result);
    if(!conv.failed) {
        ret = t2(env, atom_ok, ret);
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
nif_gc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraCtx* ctx;
    ErlChakraConv conv;
    ERL_NIF_TERM ret;
    JsErrorCode err;

    init_conv(&conv, env);

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], ErlChakraCtxRes, &res_handle)) {
        return enif_make_badarg(env);
    }
    ctx = (ErlChakraCtx*) res_handle;

    if(!check_pid(env, ctx)) {
        return enif_make_badarg(env);
    }

    err = JsSetCurrentContext(ctx->context);
    if(err != JsNoError) {
        goto done;
    }

    err = JsCollectGarbage(ctx->runtime);
    if(err != JsNoError) {
        goto done;
    }

    ret = atom_ok;

done:
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

    if(!check_pid(env, ctx)) {
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

    ret = t2(env, atom_ok, enif_make_uint(env, ticks));

done:
    if(err != JsNoError) {
        ret = js2erl_error(&conv, err);
    }

    JsSetCurrentContext(JS_INVALID_REFERENCE);
    return ret;
}


static ErlNifFunc funcs[] =
{
    {"nif_create_context", 1, nif_create_context, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"nif_run", 2, nif_run, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"nif_call", 3, nif_call, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"nif_gc", 1, nif_gc, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"nif_idle", 1, nif_idle, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

ERL_NIF_INIT(chakra, funcs, &load, NULL, NULL, &unload);


ERL_NIF_TERM
erl2js_atom(ErlChakraConv* conv, ERL_NIF_TERM obj, JsValueRef* out)
{
    char buf[ERL_CHAKRA_MAX_ATOM_LENGTH];
    size_t size;
    JsErrorCode err;

    if(enif_is_identical(obj, atom_undefined)) {
        err = JsGetUndefinedValue(out);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }
        return atom_ok;
    }

    if(enif_is_identical(obj, atom_null)) {
        err = JsGetNullValue(out);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }
        return atom_ok;
    }

    if(enif_is_identical(obj, atom_true)) {
        err = JsGetTrueValue(out);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }
        return atom_ok;
    }

    if(enif_is_identical(obj, atom_false)) {
        err = JsGetFalseValue(out);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }
        return atom_ok;
    }

    size = enif_get_atom(conv->env, obj, buf,
            ERL_CHAKRA_MAX_ATOM_LENGTH, ERL_NIF_LATIN1);
    if(size < 1) {
        return t2(conv->env, mkatom(conv->env, "invalid_atom"), obj);
    }

    err = JsCreateString(buf, size - 1, out);
    if(err != JsNoError) {
        return js2erl_error(conv, err);
    }

    return atom_ok;
}


ERL_NIF_TERM
erl2js_binary(ErlChakraConv* conv, ERL_NIF_TERM obj, JsValueRef* out)
{
    ErlNifBinary bin;
    JsErrorCode err;

    if(!enif_inspect_binary(conv->env, obj, &bin)) {
        return t2(conv->env, mkatom(conv->env, "invalid_binary"), obj);
    }

    err = JsCreateString((char*) bin.data, bin.size, out);
    if(err != JsNoError) {
        return js2erl_error(conv, err);
    }

    return atom_ok;
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

        return atom_ok;
    }

    if(enif_get_double(conv->env, obj, &dval)) {
        err = JsDoubleToNumber(dval, out);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }

        return atom_ok;
    }

    return t2(conv->env, mkatom(conv->env, "invalid_number"), obj);
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
        return t2(conv->env, mkatom(conv->env, "invalid_list"), obj);
    }

    if(length > (size_t) 2147483647) {
        return mkatom(conv->env, "invalid_list_length");
    }

    err = JsCreateArray(length, &array);
    if(err != JsNoError) {
        return js2erl_error(conv, err);
    }

    for(i = 0; i < length; i++) {
        if(!enif_get_list_cell(conv->env, obj, &cell, &obj)) {
            return t2(conv->env, mkatom(conv->env, "invalid_list"), obj);
        }

        init_conv(&sub_conv, conv->env);
        term = erl2js(&sub_conv, cell, &elem);
        if(term != atom_ok) {
            return term;
        }

        err = JsIntToNumber(i, &idx);
        if(err != JsNoError) {
            return mkatom(conv->env, "invalid_index");
        }

        err = JsSetIndexedProperty(array, idx, elem);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }
    }

    *out = array;
    return atom_ok;
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
            return t2(conv->env, mkatom(conv->env, "invalid_key"), obj);
        }

        err = JsCreatePropertyId(buf, size, out);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }

        return atom_ok;
    }

    if(enif_is_binary(conv->env, obj)) {
        if(!enif_inspect_binary(conv->env, obj, &bin)) {
            return t2(conv->env, mkatom(conv->env, "invalid_key"), obj);
        }

        err = JsCreatePropertyId((char*) bin.data, bin.size, out);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }

        return atom_ok;
    }

    return t2(conv->env, mkatom(conv->env, "invalid_key"), obj);
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
        return t2(conv->env, mkatom(conv->env, "invalid_term"), obj);
    }

    if(arity != 1) {
        return t2(conv->env, mkatom(conv->env, "invalid_term"), obj);
    }

    props = items[0];

    if(!enif_is_list(conv->env, props)) {
        return t2(conv->env, mkatom(conv->env, "invalid_term"), obj);
    }

    if(!enif_get_list_length(conv->env, props, (unsigned int*) &length)) {
        return t2(conv->env, mkatom(conv->env, "invalid_term"), obj);
    }

    err = JsCreateObject(&ret);
    if(err != JsNoError) {
        return js2erl_error(conv, err);
    }

    for(i = 0; i < length; i++) {
        if(!enif_get_list_cell(conv->env, props, &cell, &props)) {
            return t2(conv->env, mkatom(conv->env, "invalid_props"), props);
        }

        if(!enif_is_tuple(conv->env, cell)) {
            return t2(conv->env, mkatom(conv->env, "invalid_property"), cell);
        }

        if(!enif_get_tuple(conv->env, cell, &arity, &items)) {
            return t2(conv->env, mkatom(conv->env, "invalid_property"), cell);
        }

        if(arity != 2) {
            return t2(conv->env, mkatom(conv->env, "invalid_property"), cell);
        }

        elem = erl2js_prop_id(conv, items[0], &key);
        if(elem != atom_ok) {
            return elem;
        }

        elem = erl2js(conv, items[1], &val);
        if(elem != atom_ok) {
            return elem;
        }

        // TODO: Strict rules?
        err = JsSetProperty(ret, key, val, false);
        if(err != JsNoError) {
            return js2erl_error(conv, err);
        }
    }

    *out = ret;
    return atom_ok;
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

    return t2(conv->env, atom_invalid_term, obj);
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
        return atom_true;
    } else {
        return atom_false;
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
            return atom_undefined;
        case JsNull:
            return atom_null;
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
            return t2(conv->env, atom_error,
                    mkatom(conv->env, "invalid_value_type"));
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
        return t2(conv->env, atom_error, js2erl_error_code(conv, err));
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
            ret = t2(conv->env, atom_exception, ret);
        } else {
            ret = t2(conv->env, atom_error, ret);
        }
    } else {
        ret = t2(conv->env, atom_error, js2erl_error_code(conv, err));
    }

    conv->failed = true;
    return ret;
}


#define RET_ERROR(Name, Value)                      \
do {                                                \
    if(err == Name) {                               \
        return enif_make_atom(conv->env, Value);    \
    }                                               \
} while(0)


ERL_NIF_TERM
js2erl_error_code(ErlChakraConv* conv, JsErrorCode err)
{
    RET_ERROR(JsNoError, "no_error");
    RET_ERROR(JsErrorCategoryUsage, "category_usage");
    RET_ERROR(JsErrorInvalidArgument, "invalid_argument");
    RET_ERROR(JsErrorNullArgument, "null_argument");
    RET_ERROR(JsErrorNoCurrentContext, "no_current_context");
    RET_ERROR(JsErrorInExceptionState, "in_exception_state");
    RET_ERROR(JsErrorNotImplemented, "not_implemented");
    RET_ERROR(JsErrorWrongThread, "wrong_thread");
    RET_ERROR(JsErrorRuntimeInUse, "runtime_in_use");
    RET_ERROR(JsErrorBadSerializedScript, "bad_serialized_script");
    RET_ERROR(JsErrorInDisabledState, "in_disabled_state");
    RET_ERROR(JsErrorCannotDisableExecution, "cannot_disable_execution");
    RET_ERROR(JsErrorHeapEnumInProgress, "heap_enum_in_progress");
    RET_ERROR(JsErrorArgumentNotObject, "argument_not_object");
    RET_ERROR(JsErrorInProfileCallback, "in_profile_callback");
    RET_ERROR(JsErrorInThreadServiceCallback, "in_thread_service_callback");
    RET_ERROR(JsErrorCannotSerializeDebugScript, "cannot_serialize_debug_script");
    RET_ERROR(JsErrorAlreadyDebuggingContext, "already_debugging_context");
    RET_ERROR(JsErrorAlreadyProfilingContext, "already_profiling_context");
    RET_ERROR(JsErrorIdleNotEnabled, "idle_not_enabled");
    RET_ERROR(JsCannotSetProjectionEnqueueCallback, "cannot_set_projection_enqueue_callback");
    RET_ERROR(JsErrorCannotStartProjection, "cannot_start_projection");
    RET_ERROR(JsErrorInObjectBeforeCollectCallback, "in_object_before_collect_callback");
    RET_ERROR(JsErrorObjectNotInspectable, "object_not_inspectable");
    RET_ERROR(JsErrorPropertyNotSymbol, "property_not_symbol");
    RET_ERROR(JsErrorPropertyNotString, "property_not_string");
    RET_ERROR(JsErrorInvalidContext, "invalid_context");
    RET_ERROR(JsInvalidModuleHostInfoKind, "invalid_module_host_info_kind");
    RET_ERROR(JsErrorModuleParsed, "module_parsed");
    //RET_ERROR(JsNoWeakRefRequired, "no_weak_ref_required");
    //RET_ERROR(JsErrorPromisePending, "promise_pending");
    RET_ERROR(JsErrorCategoryEngine, "category_engine");
    RET_ERROR(JsErrorOutOfMemory, "out_of_memory");
    RET_ERROR(JsErrorBadFPUState, "bad_fpu_state");
    RET_ERROR(JsErrorCategoryScript, "category_script");
    RET_ERROR(JsErrorScriptException, "script_exception");
    RET_ERROR(JsErrorScriptCompile, "script_compile");
    RET_ERROR(JsErrorScriptTerminated, "script_terminated");
    RET_ERROR(JsErrorScriptEvalDisabled, "script_eval_disabled");
    RET_ERROR(JsErrorCategoryFatal, "category_fatal");
    RET_ERROR(JsErrorFatal, "fatal");
    RET_ERROR(JsErrorWrongRuntime, "wrong_runtime");
    RET_ERROR(JsErrorCategoryDiagError, "category_diag_error");
    RET_ERROR(JsErrorDiagAlreadyInDebugMode, "diag_already_in_debug_mode");
    RET_ERROR(JsErrorDiagNotInDebugMode, "diag_not_in_debug_mode");
    RET_ERROR(JsErrorDiagNotAtBreak, "diag_not_at_break");
    RET_ERROR(JsErrorDiagInvalidHandle, "diag_invalid_handle");
    RET_ERROR(JsErrorDiagObjectNotFound, "diag_object_not_found");
    RET_ERROR(JsErrorDiagUnableToPerformAction, "diag_unable_to_perform_action");

    return atom_unknown;
}

#undef RET_ERROR
