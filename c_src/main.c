
#include "erl_nif.h"

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    return load(env, priv, info);
}

static void
unload(ErlNifEnv* env, void* priv)
{
    return;
}


static ERL_NIF_TERM
nif_create_runtime(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_badarg(env);
}


static ErlNifFunc funcs[] =
{
    {"nif_create_runtime", 1, nif_create_runtime, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};

ERL_NIF_INIT(chakra, funcs, &load, NULL, &upgrade, &unload);