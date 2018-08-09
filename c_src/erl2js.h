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

#ifndef ERL_CHAKRA_ERL2JS_H
#define ERL_CHAKRA_ERL2JS_H


#include "erl_nif.h"
#include "ChakraCore.h"

#include "atoms.h"


ERL_NIF_TERM erl2js(ErlNifEnv* env, ERL_NIF_TERM obj, JsValueRef* out);

ERL_NIF_TERM erl2js_prop_id(
        ErlNifEnv* env,
        ERL_NIF_TERM obj,
        JsPropertyIdRef* out
    );

ERL_NIF_TERM erl2js_func(
        ErlNifEnv* env,
        ERL_NIF_TERM prop_names,
        JsValueRef obj,
        JsValueRef undefined,
        JsValueRef* out
    );

#endif // Included erl2js.h