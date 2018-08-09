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

#ifndef CHAKRA_RESOURCES_H
#define CHAKRA_RESOURCES_H

#include "erl_nif.h"
#include "ChakraCore.h"


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
    ERL_NIF_TERM source;
    ERL_NIF_TERM serialized;
    JsParseScriptAttributes attrs;
} ErlChakraSerializedScript;


void erl_chakra_rt_dtor(ErlNifEnv* env, void* obj);
void erl_chakra_ctx_dtor(ErlNifEnv* env, void* obj);
void erl_chakra_serialized_script_dtor(ErlNifEnv* env, void* obj);


#endif // Included resources.h