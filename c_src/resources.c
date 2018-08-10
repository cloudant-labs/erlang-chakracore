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

#include "resources.h"
#include "runtime.h"

ErlNifResourceType* ErlChakraRtRes;
ErlNifResourceType* ErlChakraCtxRes;
ErlNifResourceType* ErlChakraScriptRes;


int
erl_chakra_init_resources(ErlNifEnv* env)
{

    ErlChakraRtRes = enif_open_resource_type(
            env,
            NULL,
            "erl_chakra_runtime",
            erl_chakra_rt_dtor,
            ERL_NIF_RT_CREATE,
            NULL
        );
    if(ErlChakraRtRes == NULL) {
        return 0;
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
        return 0;
    }

    ErlChakraScriptRes = enif_open_resource_type(
            env,
            NULL,
            "erl_chakra_serialized_script",
            erl_chakra_script_dtor,
            ERL_NIF_RT_CREATE,
            NULL
        );
    if(ErlChakraScriptRes == NULL) {
        return 0;
    }

    return 1;
}

void
erl_chakra_rt_dtor(ErlNifEnv* env, void* obj)
{
    ErlChakraRt* rt = (ErlChakraRt*) obj;

    if(rt->runtime == JS_INVALID_RUNTIME_HANDLE) {
        return;
    }

    erl_chakra_runtime_destroy(rt);
}


void
erl_chakra_ctx_dtor(ErlNifEnv* env, void* obj)
{
    ErlChakraCtx* ctx = (ErlChakraCtx*) obj;

    if(ctx->context != JS_INVALID_REFERENCE) {
        JsRelease(ctx->context, NULL);
    }

    enif_release_resource(ctx->rt);

    return;
}


void
erl_chakra_script_dtor(ErlNifEnv* env, void* obj)
{
    ErlChakraScript* script = (ErlChakraScript*) obj;
    enif_free_env(script->env);
}