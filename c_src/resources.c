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


void
erl_chakra_serialized_script_dtor(ErlNifEnv* env, void* obj)
{
    ErlChakraSerializedScript* script = (ErlChakraSerializedScript*) obj;
    enif_free_env(script->env);
}