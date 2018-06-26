
#include <string.h>

#include "erl_nif.h"
#include "ChakraCore.h"


#define ERL_CHAKRA_OK 0
#define ERL_CHAKRA_ERROR 1



ERL_NIF_TERM atom_ok;
ERL_NIF_TERM atom_error;
ERL_NIF_TERM atom_exception;
ERL_NIF_TERM atom_unknown;


ErlNifResourceType* ErlChakraCtxRes;

typedef struct {
    JsRuntimeHandle runtime;
    JsContextRef context;
    JsSourceContext source_ctx;
} ErlChakraCtx;


int js_to_erl(
        ErlNifEnv* env,
        ErlChakraCtx* ctx,
        JsValueRef obj,
        ERL_NIF_TERM* out
    );

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

        if(js_to_erl(env, ctx, result, &ret) == ERL_CHAKRA_OK) {
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

    if(js_to_erl(env, ctx, result, &ret) == ERL_CHAKRA_OK) {
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
    {"nif_gc", 1, nif_gc, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

ERL_NIF_INIT(chakra, funcs, &load, NULL, NULL, &unload);


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

    return enif_make_atom(env, "unknown");
}

#undef RET_ERROR


int
js_to_erl(ErlNifEnv* env, ErlChakraCtx* ctx, JsValueRef obj, ERL_NIF_TERM* out)
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