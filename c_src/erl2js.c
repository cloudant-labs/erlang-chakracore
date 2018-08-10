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

#include "atoms.h"
#include "erl2js.h"
#include "js2erl.h"
#include "util.h"


#define RET_ERROR(CALL)                             \
do {                                                \
    ERL_NIF_TERM _tmp;                              \
    JsErrorCode err = CALL;                         \
    if(err != JsNoError) {                          \
        js2erl_error(env, err, &_tmp);              \
        return _tmp;                                \
    }                                               \
} while(0)


ERL_NIF_TERM
erl2js_atom(ErlNifEnv* env, ERL_NIF_TERM obj, JsValueRef* out)
{
    char buf[ERL_CHAKRA_MAX_ATOM_LENGTH];
    size_t size;

    if(enif_is_identical(obj, ATOM_undefined)) {
        RET_ERROR(JsGetUndefinedValue(out));
        return ATOM_ok;
    }

    if(enif_is_identical(obj, ATOM_null)) {
        RET_ERROR(JsGetNullValue(out));
        return ATOM_ok;
    }

    if(enif_is_identical(obj, ATOM_true)) {
        RET_ERROR(JsGetTrueValue(out));
        return ATOM_ok;
    }

    if(enif_is_identical(obj, ATOM_false)) {
        RET_ERROR(JsGetFalseValue(out));
        return ATOM_ok;
    }

    size = enif_get_atom(env, obj, buf,
            ERL_CHAKRA_MAX_ATOM_LENGTH, ERL_NIF_LATIN1);
    if(size < 1) {
        return T2(env, ATOM_invalid_atom, obj);
    }

    RET_ERROR(JsCreateString(buf, size - 1, out));
    return ATOM_ok;
}


ERL_NIF_TERM
erl2js_binary(ErlNifEnv* env, ERL_NIF_TERM obj, JsValueRef* out)
{
    ErlNifBinary bin;

    if(!enif_inspect_binary(env, obj, &bin)) {
        return T2(env, ATOM_invalid_binary, obj);
    }

    RET_ERROR(JsCreateString((char*) bin.data, bin.size, out));

    return ATOM_ok;
}


ERL_NIF_TERM
erl2js_number(ErlNifEnv* env, ERL_NIF_TERM obj, JsValueRef* out)
{
    int ival;
    double dval;

    if(enif_get_int(env, obj, &ival)) {
        RET_ERROR(JsIntToNumber(ival, out));
        return ATOM_ok;
    }

    if(enif_get_double(env, obj, &dval)) {
        RET_ERROR(JsDoubleToNumber(dval, out));
        return ATOM_ok;
    }

    return T2(env, ATOM_invalid_number, obj);
}


ERL_NIF_TERM
erl2js_list(ErlNifEnv* env, ERL_NIF_TERM obj, JsValueRef* out)
{
    ERL_NIF_TERM cell;
    ERL_NIF_TERM term;

    JsValueRef array;
    JsValueRef idx;
    JsValueRef elem;

    unsigned int length;
    int i;

    if(!enif_get_list_length(env, obj, &length)) {
        return T2(env, ATOM_invalid_list, obj);
    }

    if(length > (size_t) 2147483647) {
        return ATOM_invalid_list_length;
    }

    RET_ERROR(JsCreateArray(length, &array));
    for(i = 0; i < length; i++) {
        if(!enif_get_list_cell(env, obj, &cell, &obj)) {
            return T2(env, ATOM_invalid_list, obj);
        }

        term = erl2js(env, cell, &elem);
        if(term != ATOM_ok) {
            return term;
        }

        RET_ERROR(JsIntToNumber(i, &idx));
        RET_ERROR(JsSetIndexedProperty(array, idx, elem));
    }

    *out = array;
    return ATOM_ok;
}


ERL_NIF_TERM
erl2js_prop_id(ErlNifEnv* env, ERL_NIF_TERM obj, JsPropertyIdRef* out)
{
    char buf[ERL_CHAKRA_MAX_ATOM_LENGTH];
    size_t size;
    ErlNifBinary bin;

    if(enif_is_atom(env, obj)) {
        size = enif_get_atom(env, obj, buf,
                ERL_CHAKRA_MAX_ATOM_LENGTH, ERL_NIF_LATIN1);
        if(size < 1) {
            return T2(env, ATOM_invalid_key, obj);
        }

        RET_ERROR(JsCreatePropertyId(buf, size - 1, out));
        return ATOM_ok;
    }

    if(enif_is_binary(env, obj)) {
        if(!enif_inspect_binary(env, obj, &bin)) {
            return T2(env, ATOM_invalid_key, obj);
        }

        RET_ERROR(JsCreatePropertyId((char*) bin.data, bin.size, out));
        return ATOM_ok;
    }

    return T2(env, ATOM_invalid_key, obj);
}


ERL_NIF_TERM
erl2js_func(
        ErlNifEnv* env,
        ERL_NIF_TERM prop_names,
        JsValueRef obj,
        JsValueRef undefined,
        JsValueRef* out
    )
{
    ERL_NIF_TERM name;
    ERL_NIF_TERM rest_names;
    ERL_NIF_TERM ret;
    JsPropertyIdRef fname;
    bool is_undefined;

    if(!enif_is_list(env, prop_names)) {
        return enif_make_badarg(env);
    }

    if(enif_is_empty_list(env, prop_names)) {
        return enif_make_badarg(env);
    }

    if(!enif_get_list_cell(env, prop_names, &name, &rest_names)) {
        return enif_make_badarg(env);
    }

    ret = erl2js_prop_id(env, name, &fname);
    if(ret != ATOM_ok) {
        return ret;
    }

    RET_ERROR(JsGetProperty(obj, fname, out));
    RET_ERROR(JsEquals(*out, undefined, &is_undefined));

    if(is_undefined) {
        return T2(env, ATOM_error, ATOM_undefined_function);
    }

    if(enif_is_empty_list(env, rest_names)) {
        return ATOM_ok;
    }

    return erl2js_func(env, rest_names, *out, undefined, out);
}


ERL_NIF_TERM
erl2js_object(ErlNifEnv* env, ERL_NIF_TERM obj, JsValueRef* out)
{
    const ERL_NIF_TERM* items;
    ERL_NIF_TERM props;
    ERL_NIF_TERM cell;
    ERL_NIF_TERM elem;

    JsValueRef ret;
    JsValueRef val;
    JsPropertyIdRef key;

    int arity;
    size_t length;
    size_t i;

    if(!enif_get_tuple(env, obj, &arity, &items)) {
        return T2(env, ATOM_invalid_term, obj);
    }

    if(arity != 1) {
        return T2(env, ATOM_invalid_term, obj);
    }

    props = items[0];

    if(!enif_is_list(env, props)) {
        return T2(env, ATOM_invalid_term, obj);
    }

    if(!enif_get_list_length(env, props, (unsigned int*) &length)) {
        return T2(env, ATOM_invalid_term, obj);
    }

    RET_ERROR(JsCreateObject(&ret));

    for(i = 0; i < length; i++) {
        if(!enif_get_list_cell(env, props, &cell, &props)) {
            return T2(env, ATOM_invalid_props, props);
        }

        if(!enif_is_tuple(env, cell)) {
            return T2(env, ATOM_invalid_property, cell);
        }

        if(!enif_get_tuple(env, cell, &arity, &items)) {
            return T2(env, ATOM_invalid_property, cell);
        }

        if(arity != 2) {
            return T2(env, ATOM_invalid_property, cell);
        }

        elem = erl2js_prop_id(env, items[0], &key);
        if(elem != ATOM_ok) {
            return elem;
        }

        elem = erl2js(env, items[1], &val);
        if(elem != ATOM_ok) {
            return elem;
        }

        // TODO: Strict rules?
        RET_ERROR(JsSetProperty(ret, key, val, false));
    }

    *out = ret;
    return ATOM_ok;
}


ERL_NIF_TERM
erl2js(ErlNifEnv* env, ERL_NIF_TERM obj, JsValueRef* out)
{
    if(enif_is_atom(env, obj)) {
        return erl2js_atom(env, obj, out);
    }

    if(enif_is_binary(env, obj)) {
        return erl2js_binary(env, obj, out);
    }

    if(enif_is_number(env, obj)) {
        return erl2js_number(env, obj, out);
    }

    if(enif_is_list(env, obj)) {
        return erl2js_list(env, obj, out);
    }

    if(enif_is_tuple(env, obj)) {
        return erl2js_object(env, obj, out);
    }

    return T2(env, ATOM_invalid_term, obj);
}
