
#include <string.h>

#include "erl_nif.h"
#include "ChakraCore.h"

ERL_NIF_TERM atom_ok;
ERL_NIF_TERM atom_error;
ERL_NIF_TERM atom_unknown;


ErlNifResourceType* ErlChakraCtxRes;

typedef struct {
    JsRuntimeHandle runtime;
    JsContextRef context;
    JsSourceContext source_ctx;
} ErlChakraCtx;


void
erl_chakra_ctx_dtor(ErlNifEnv* env, void* obj)
{
    ErlChakraCtx* ctx = (ErlChakraCtx*) obj;

    if(ctx->runtime != NULL) {
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

    err = JsCreateRuntime(JsRuntimeAttributeNone, NULL, &(ctx->runtime));
    if(err != JsNoError) {
        ctx->runtime = NULL;
        enif_release_resource(ctx);
        return enif_make_badarg(env);
    }

    err = JsCreateContext(ctx->runtime, &(ctx->context));
    if(err != JsNoError) {
        ctx->context = NULL;
        enif_release_resource(ctx);
        return enif_make_badarg(env);
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
    JsValueRef js_result;
    JsValueRef str_result;
    JsErrorCode err;
    ERL_NIF_TERM erl_result;
    unsigned char* res_buf;
    size_t res_len;
    ERL_NIF_TERM ret;

    ret = enif_make_tuple2(env, atom_error, atom_unknown);

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
        goto done;
    }

    err = JsCreateString("<ERLANG>", strlen("<ERLANG>"), &script_name);
    if(err != JsNoError) {
        goto done;
    }

    err = JsCreateExternalArrayBuffer(src.data, src.size, NULL, NULL, &script);
    if(err != JsNoError) {
        goto done;
    }

    err = JsRun(
            script,
            ctx->source_ctx++,
            script_name,
            JsParseScriptAttributeNone,
            &js_result
        );
    if(err != JsNoError) {
        goto done;
    }

    err = JsConvertValueToString(js_result, &str_result);
    if(err != JsNoError) {
        goto done;
    }

    err = JsCopyString(str_result, NULL, 0, &res_len);
    if(err != JsNoError) {
        goto done;
    }

    res_buf = enif_make_new_binary(env, res_len, &erl_result);
    err = JsCopyString(str_result, (char*) res_buf, res_len, NULL);
    if(err != JsNoError) {
        goto done;
    }

    ret = enif_make_tuple2(env, atom_ok, erl_result);

done:
    JsSetCurrentContext(JS_INVALID_REFERENCE);
    return ret;
}


static ErlNifFunc funcs[] =
{
    {"nif_create_context", 1, nif_create_context, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"nif_run", 2, nif_run, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

ERL_NIF_INIT(chakra, funcs, &load, NULL, NULL, &unload);