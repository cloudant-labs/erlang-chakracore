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

#ifndef ERL_CHAKRA_RUNTIME_H
#define ERL_CHAKRA_RUNTIME_H


#include "erl_nif.h"
#include "ChakraCore.h"

#include "resources.h"


ErlChakraJob* erl_chakra_job_create();
void erl_chakra_job_destroy(ErlChakraJob* job);


ERL_NIF_TERM erl_chakra_runtime_create(
        ErlNifEnv* env,
        JsRuntimeAttributes attrs,
        int memory_limit,
        int stack_size
    );

void erl_chakra_runtime_destroy(ErlChakraRt* rt);

bool erl_chakra_runtime_submit(ErlChakraRt* rt, ErlChakraJob* job);



#endif // Included runtime.h