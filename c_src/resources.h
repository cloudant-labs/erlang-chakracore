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


extern ErlNifResourceType* ErlChakraRtRes;
extern ErlNifResourceType* ErlChakraCtxRes;
extern ErlNifResourceType* ErlChakraScriptRes;


typedef struct {
    JsContextRef context;
    JsSourceContext source_ctx;
} ErlChakraCtx;


typedef enum {
    ERL_CHAKRA_JOB_TYPE_UNKNOWN,
    ERL_CHAKRA_JOB_TYPE_CLOSE,
    ERL_CHAKRA_JOB_TYPE_GC,
    ERL_CHAKRA_JOB_TYPE_ENABLE,
    ERL_CHAKRA_JOB_TYPE_DISABLE,
    ERL_CHAKRA_JOB_TYPE_CREATE_CTX,
    ERL_CHAKRA_JOB_TYPE_SERIALIZE,
    ERL_CHAKRA_JOB_TYPE_RUN,
    ERL_CHAKRA_JOB_TYPE_CALL,
    ERL_CHAKRA_JOB_TYPE_IDLE
} ErlChakraJobType;


typedef struct _ErlChakraJob
{
    ErlNifEnv* env;
    ERL_NIF_TERM ref;

    ErlChakraJobType type;
    ErlChakraCtx* ctx;

    ERL_NIF_TERM args[2];
} ErlChakraJob;


typedef struct {
    JsRuntimeHandle runtime;

    ErlNifPid pid;

    ErlNifTid tid;
    ErlNifMutex* lock;
    ErlNifCond* cond;
    ErlChakraJob* job;

    bool allow_script_interrupt;

    bool alive;
} ErlChakraRt;


typedef struct {
    ErlNifEnv* env;
    ERL_NIF_TERM source;
    ERL_NIF_TERM serialized;
    JsParseScriptAttributes attrs;
} ErlChakraScript;


int erl_chakra_init_resources(ErlNifEnv* env);
void erl_chakra_rt_dtor(ErlNifEnv* env, void* obj);
void erl_chakra_ctx_dtor(ErlNifEnv* env, void* obj);
void erl_chakra_script_dtor(ErlNifEnv* env, void* obj);


#endif // Included resources.h