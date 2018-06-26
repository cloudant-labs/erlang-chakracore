
#include <string.h>

#include "erl_nif.h"
#include "ChakraCore.h"


#define ERL_CHAKRA_MAX_ATOM_LENGTH 255

#define ERL_CHAKRA_OK 0
#define ERL_CHAKRA_ERROR 1



ERL_NIF_TERM atom_ok;
ERL_NIF_TERM atom_error;
ERL_NIF_TERM atom_exception;
ERL_NIF_TERM atom_invalid_term;
ERL_NIF_TERM atom_unknown;


ErlNifResourceType* ErlChakraCtxRes;

typedef struct {
    JsRuntimeHandle runtime;
    JsContextRef context;
    JsSourceContext source_ctx;
} ErlChakraCtx;


ERL_NIF_TERM tuple(ErlNifEnv* env, ERL_NIF_TERM a, ERL_NIF_TERM b);
ERL_NIF_TERM erl_to_js(ErlNifEnv* env, ERL_NIF_TERM obj, JsValueRef* out);
ERL_NIF_TERM erl_key_to_js_prop_id(ErlNifEnv* env, ERL_NIF_TERM obj, JsPropertyIdRef* out);
int js_to_erl(ErlNifEnv* env, JsValueRef obj, ERL_NIF_TERM* out);
ERL_NIF_TERM js_to_erl_error(ErlNifEnv* env, JsErrorCode err);
ERL_NIF_TERM js_error_tuple(ErlNifEnv* env, JsErrorCode err);


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
    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");
    atom_exception = enif_make_atom(env, "exception");
    atom_invalid_term = enif_make_atom(env, "invalid_term");
    atom_unknown = enif_make_atom(env, "unknown");

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
    ERL_NIF_TERM ret;
    JsErrorCode err;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_is_list(env, argv[0])) {
        return enif_make_badarg(env);
    }

    ctx = enif_alloc_resource(ErlChakraCtxRes, sizeof(ErlChakraCtx));
    ctx->runtime = JS_INVALID_RUNTIME_HANDLE;

    err = JsCreateRuntime(JsRuntimeAttributeNone, NULL, &(ctx->runtime));
    if(err != JsNoError) {
        ctx->runtime = JS_INVALID_RUNTIME_HANDLE;
        enif_release_resource(ctx);
        return js_error_tuple(env, err);
    }

    err = JsCreateContext(ctx->runtime, &(ctx->context));
    if(err != JsNoError) {
        enif_release_resource(ctx);
        return js_error_tuple(env, err);
    }

    err = JsAddRef(ctx->context, NULL);
    if(err != JsNoError) {
        enif_release_resource(ctx);
        return js_error_tuple(env, err);
    }

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
    ErlNifBinary src;
    JsValueRef script_name;
    JsValueRef script;
    JsValueRef result;
    JsErrorCode err;
    ERL_NIF_TERM ret;

    if(argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], ErlChakraCtxRes, &res_handle)) {
        return enif_make_badarg(env);
    }
    ctx = (ErlChakraCtx*) res_handle;

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

    if(err == JsErrorScriptException) {
        err = JsGetAndClearException(&result);
        if(err != JsNoError) {
            goto error;
        }

        if(js_to_erl(env, result, &ret) == ERL_CHAKRA_OK) {
            ret = enif_make_tuple2(env, atom_exception, ret);
        } else {
            ret = enif_make_tuple2(env, atom_error, ret);
        }

        JsSetCurrentContext(JS_INVALID_REFERENCE);
        return ret;
    }

    if(err != JsNoError) {
        goto error;
    }

    if(js_to_erl(env, result, &ret) == ERL_CHAKRA_OK) {
        ret = enif_make_tuple2(env, atom_ok, ret);
    } else {
        ret = enif_make_tuple2(env, atom_error, ret);
    }

    JsSetCurrentContext(JS_INVALID_REFERENCE);
    return ret;

error:
    JsSetCurrentContext(JS_INVALID_REFERENCE);
    return js_error_tuple(env, err);
}


static ERL_NIF_TERM
nif_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraCtx* ctx;
    ERL_NIF_TERM list;
    ERL_NIF_TERM cell;
    JsValueRef undefined;
    bool is_undefined;
    JsValueRef global_obj;
    JsPropertyIdRef fname;
    JsValueRef func;
    size_t length;
    size_t i;
    JsValueRef* args = NULL;
    JsValueRef result;
    ERL_NIF_TERM ret;
    JsErrorCode err;

    if(argc != 3) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], ErlChakraCtxRes, &res_handle)) {
        return enif_make_badarg(env);
    }
    ctx = (ErlChakraCtx*) res_handle;

    err = JsSetCurrentContext(ctx->context);
    if(err != JsNoError) {
        return js_error_tuple(env, err);
    }

    ret = erl_key_to_js_prop_id(env, argv[1], &fname);
    if(ret != atom_ok) {
        return tuple(env, atom_error,
                enif_make_atom(env, "invalid_function_name"));
    }

    err = JsGetUndefinedValue(&undefined);
    if(err != JsNoError) {
        return js_to_erl_error(env, err);
    }

    err = JsGetGlobalObject(&global_obj);
    if(err != JsNoError) {
        return js_to_erl_error(env, err);
    }

    err = JsGetProperty(global_obj, fname, &func);
    if(err != JsNoError) {
        return js_to_erl_error(env, err);
    }

    err = JsEquals(func, undefined, &is_undefined);
    if(err != JsNoError) {
        return js_to_erl_error(env, err);
    }

    if(is_undefined) {
        return tuple(env, atom_error,
                enif_make_atom(env, "undefined_function"));
    }

    list = argv[2];
    if(!enif_is_list(env, list)) {
        return tuple(env, atom_error,
                enif_make_atom(env, "invalid_argument_list"));
    }

    if(!enif_get_list_length(env, list, (unsigned int*) &length)) {
        return tuple(env, atom_error,
                enif_make_atom(env, "invalid_argument_list"));
    }

    args = enif_alloc((length + 1) * sizeof(JsValueRef));
    args[0] = global_obj;
    for(i = 1; i <= length; i++) {
        if(!enif_get_list_cell(env, list, &cell, &list)) {
            return tuple(env, atom_error,
                    enif_make_atom(env, "invalid_argument_list"));
        }

        ret = erl_to_js(env, cell, &(args[i]));
        if(ret != atom_ok) {
            goto done;
        }
    }

    err = JsCallFunction(func, args, length + 1, &result);

    if(err == JsErrorScriptException) {
        err = JsGetAndClearException(&result);
        if(err != JsNoError) {
            goto done;
        }

        if(js_to_erl(env, result, &ret) == ERL_CHAKRA_OK) {
            ret = enif_make_tuple2(env, atom_exception, ret);
        } else {
            ret = enif_make_tuple2(env, atom_error, ret);
        }

        goto done;
    }

    if(err != JsNoError) {
        goto done;
    }

    if(js_to_erl(env, result, &ret) == ERL_CHAKRA_OK) {
        ret = tuple(env, atom_ok, ret);
    } else {
        ret = tuple(env, atom_error, ret);
    }

done:
    if(args != NULL) {
        enif_free(args);
    }

    JsSetCurrentContext(JS_INVALID_REFERENCE);
    return ret;
}



static ERL_NIF_TERM
nif_gc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* res_handle;
    ErlChakraCtx* ctx;
    JsErrorCode err;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], ErlChakraCtxRes, &res_handle)) {
        return enif_make_badarg(env);
    }

    ctx = (ErlChakraCtx*) res_handle;

    err = JsCollectGarbage(ctx->runtime);
    if(err != JsNoError) {
        return js_error_tuple(env, err);
    }

    return atom_ok;
}


static ErlNifFunc funcs[] =
{
    {"nif_create_context", 1, nif_create_context, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"nif_run", 2, nif_run, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"nif_call", 3, nif_call, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"nif_gc", 1, nif_gc, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

ERL_NIF_INIT(chakra, funcs, &load, NULL, NULL, &unload);


ERL_NIF_TERM
tuple(ErlNifEnv* env, ERL_NIF_TERM a, ERL_NIF_TERM b)
{
    return enif_make_tuple2(env, a, b);
}


ERL_NIF_TERM
erl_atom_to_js(ErlNifEnv* env, ERL_NIF_TERM obj, JsValueRef* out)
{
    char buf[ERL_CHAKRA_MAX_ATOM_LENGTH];
    size_t size;
    JsErrorCode err;

    size = enif_get_atom(env, obj, buf,
            ERL_CHAKRA_MAX_ATOM_LENGTH, ERL_NIF_LATIN1);
    if(size < 1) {
        return tuple(env, enif_make_atom(env, "invalid_atom"), obj);
    }

    err = JsCreateString(buf, size - 1, out);
    if(err != JsNoError) {
        return js_to_erl_error(env, err);
    }

    return atom_ok;
}


ERL_NIF_TERM
erl_binary_to_js(ErlNifEnv* env, ERL_NIF_TERM obj, JsValueRef* out)
{
    ErlNifBinary bin;
    JsErrorCode err;

    if(!enif_inspect_binary(env, obj, &bin)) {
        return tuple(env, enif_make_atom(env, "invalid_binary"), obj);
    }

    err = JsCreateString((char*) bin.data, bin.size, out);
    if(err != JsNoError) {
        return js_to_erl_error(env, err);
    }

    return atom_ok;
}


ERL_NIF_TERM
erl_number_to_js(ErlNifEnv* env, ERL_NIF_TERM obj, JsValueRef* out)
{
    int ival;
    double dval;
    JsErrorCode err;

    if(enif_get_int(env, obj, &ival)) {
        err = JsIntToNumber(ival, out);
        if(err != JsNoError) {
            return js_to_erl_error(env, err);
        }

        return atom_ok;
    }

    if(enif_get_double(env, obj, &dval)) {
        err = JsDoubleToNumber(dval, out);
        if(err != JsNoError) {
            return js_to_erl_error(env, err);
        }

        return atom_ok;
    }

    return tuple(env, enif_make_atom(env, "invalid_number"), obj);
}


ERL_NIF_TERM
erl_list_to_js(ErlNifEnv* env, ERL_NIF_TERM obj, JsValueRef* out)
{
    ERL_NIF_TERM cell;
    ERL_NIF_TERM term;
    size_t length;
    JsValueRef array;
    JsValueRef idx;
    JsValueRef elem;
    int i;
    JsErrorCode err;

    if(!enif_get_list_length(env, obj, (unsigned int*) &length)) {
        return tuple(env, enif_make_atom(env, "invalid_list"), obj);
    }

    if(length > (size_t) 2147483647) {
        return enif_make_atom(env, "invalid_list_length");
    }

    err = JsCreateArray(length, &array);
    if(err != JsNoError) {
        return js_to_erl_error(env, err);
    }

    for(i = 0; i < length; i++) {
        if(!enif_get_list_cell(env, obj, &cell, &obj)) {
            return tuple(env, enif_make_atom(env, "invalid_list"), obj);
        }

        term = erl_to_js(env, cell, &elem);
        if(term != atom_ok) {
            return term;
        }

        err = JsIntToNumber(i, &idx);
        if(err != JsNoError) {
            return enif_make_atom(env, "invalid_index");
        }

        err = JsSetIndexedProperty(array, idx, elem);
        if(err != JsNoError) {
            return js_to_erl_error(env, err);
        }
    }

    *out = array;
    return atom_ok;
}


ERL_NIF_TERM
erl_key_to_js_prop_id(ErlNifEnv* env, ERL_NIF_TERM obj, JsPropertyIdRef* out)
{
    char buf[ERL_CHAKRA_MAX_ATOM_LENGTH];
    size_t size;
    ErlNifBinary bin;
    JsErrorCode err;

    if(enif_is_atom(env, obj)) {
        size = enif_get_atom(env, obj, buf,
                ERL_CHAKRA_MAX_ATOM_LENGTH, ERL_NIF_LATIN1);
        if(size < 1) {
            return tuple(env, enif_make_atom(env, "invalid_key"), obj);
        }

        err = JsCreatePropertyId(buf, size, out);
        if(err != JsNoError) {
            return js_to_erl_error(env, err);
        }

        return atom_ok;
    }

    if(enif_is_binary(env, obj)) {
        if(!enif_inspect_binary(env, obj, &bin)) {
            return tuple(env, enif_make_atom(env, "invalid_key"), obj);
        }

        err = JsCreatePropertyId((char*) bin.data, bin.size, out);
        if(err != JsNoError) {
            return js_to_erl_error(env, err);
        }

        return atom_ok;
    }

    return tuple(env, enif_make_atom(env, "invalid_key"), obj);
}


ERL_NIF_TERM
erl_object_to_js(ErlNifEnv* env, ERL_NIF_TERM obj, JsValueRef* out)
{
    const ERL_NIF_TERM* items;
    int arity;
    ERL_NIF_TERM props;
    ERL_NIF_TERM cell;
    ERL_NIF_TERM elem;
    JsValueRef ret;
    JsPropertyIdRef key;
    JsValueRef val;
    size_t length;
    size_t i;
    JsErrorCode err;

    if(!enif_get_tuple(env, obj, &arity, &items)) {
        return tuple(env, enif_make_atom(env, "invalid_object"), obj);
    }

    if(arity != 1) {
        return tuple(env, enif_make_atom(env, "invalid_object"), obj);
    }

    props = items[0];

    if(!enif_is_list(env, props)) {
        return tuple(env, enif_make_atom(env, "invalid_object"), obj);
    }

    if(!enif_get_list_length(env, props, (unsigned int*) &length)) {
        return tuple(env, enif_make_atom(env, "invalid_object"), obj);
    }

    err = JsCreateObject(&ret);
    if(err != JsNoError) {
        return js_to_erl_error(env, err);
    }

    for(i = 0; i < length; i++) {
        if(!enif_get_list_cell(env, props, &cell, &props)) {
            return tuple(env, enif_make_atom(env, "invalid_props"), props);
        }

        if(!enif_is_tuple(env, cell)) {
            return tuple(env, enif_make_atom(env, "invalid_property"), cell);
        }

        if(!enif_get_tuple(env, cell, &arity, &items)) {
            return tuple(env, enif_make_atom(env, "invalid_property"), cell);
        }

        if(arity != 2) {
            return tuple(env, enif_make_atom(env, "invalid_property"), cell);
        }

        elem = erl_key_to_js_prop_id(env, items[0], &key);
        if(elem != atom_ok) {
            return elem;
        }

        elem = erl_to_js(env, items[1], &val);
        if(elem != atom_ok) {
            return elem;
        }

        // TODO: Strict rules?
        err = JsSetProperty(ret, key, val, false);
        if(err != JsNoError) {
            return js_to_erl_error(env, err);
        }
    }

    *out = ret;
    return atom_ok;
}


ERL_NIF_TERM
erl_to_js(ErlNifEnv* env, ERL_NIF_TERM obj, JsValueRef* out)
{
    // enif_is_map?
    // enif_is_pid?

    if(enif_is_atom(env, obj)) {
        return erl_atom_to_js(env, obj, out);
    }

    if(enif_is_binary(env, obj)) {
        return erl_binary_to_js(env, obj, out);
    }

    if(enif_is_number(env, obj)) {
        return erl_number_to_js(env, obj, out);
    }

    if(enif_is_list(env, obj)) {
        return erl_list_to_js(env, obj, out);
    }

    if(enif_is_tuple(env, obj)) {
        return erl_object_to_js(env, obj, out);
    }

    return tuple(env, atom_invalid_term, obj);
}


int
js_to_erl(ErlNifEnv* env, JsValueRef obj, ERL_NIF_TERM* out)
{
    JsValueRef str;
    size_t length;
    ERL_NIF_TERM result;
    unsigned char* buf;
    JsErrorCode err;

    err = JsConvertValueToString(obj, &str);
    if(err != JsNoError) {
        *out = js_to_erl_error(env, err);
        return ERL_CHAKRA_ERROR;
    }

    err = JsCopyString(str, NULL, 0, &length);
    if(err != JsNoError) {
        *out = js_to_erl_error(env, err);
        return ERL_CHAKRA_ERROR;
    }

    buf = enif_make_new_binary(env, length, &result);
    err = JsCopyString(str, (char*) buf, length, NULL);
    if(err != JsNoError) {
        *out = js_to_erl_error(env, err);
        return ERL_CHAKRA_ERROR;
    }

    *out = result;
    return ERL_CHAKRA_OK;
}


ERL_NIF_TERM
js_error_tuple(ErlNifEnv* env, JsErrorCode err) {
    return enif_make_tuple2(env, atom_error, js_to_erl_error(env, err));
}


#define RET_ERROR(Name, Value) \
do {                                        \
    if(err == Name) {                       \
        return enif_make_atom(env, Value);  \
    }                                       \
} while(0)


ERL_NIF_TERM
js_to_erl_error(ErlNifEnv* env, JsErrorCode err)
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
