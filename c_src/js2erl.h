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

#ifndef ERL_CHAKRA_JS2ERL_H
#define ERL_CHAKRA_JS2ERL_H


#include "erl_nif.h"
#include "ChakraCore.h"

#include "atoms.h"


bool js2erl(ErlNifEnv* env, JsValueRef obj, ERL_NIF_TERM* out);
bool js2erl_error(ErlNifEnv* env, JsErrorCode err, ERL_NIF_TERM* out);
ERL_NIF_TERM js2erl_error_code(JsErrorCode err);

#endif // Included js2erl.h