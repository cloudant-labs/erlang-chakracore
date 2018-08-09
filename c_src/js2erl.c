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

#include <math.h>
#include <string.h>

#include "js2erl.h"
#include "util.h"


#define RET_ERROR(CALL)                                 \
do {                                                    \
    JsErrorCode err = CALL;                             \
    if(err != JsNoError) {                              \
        return _js2erl_error(env, err, out, do_exc);    \
    }                                                   \
} while(0)


bool _js2erl(ErlNifEnv* env, JsValueRef obj, ERL_NIF_TERM* out, bool do_exc);

bool _js2erl_error(
        ErlNifEnv* env,
        JsErrorCode error,
        ERL_NIF_TERM* out,
        bool do_exc
    );



bool
js2erl_number(ErlNifEnv* env, JsValueRef obj, ERL_NIF_TERM* out, bool do_exc)
{
    double dval;
    ErlNifSInt64 ival;

    RET_ERROR(JsNumberToDouble(obj, &dval));

    if(dval == floor(dval) && !isinf(dval)) {
        ival = (ErlNifSInt64) dval;
        *out = enif_make_int64(env, ival);
        return true;
    } else {
        *out = enif_make_double(env, dval);
        return true;
    }
}


bool
js2erl_string(ErlNifEnv* env, JsValueRef obj, ERL_NIF_TERM* out, bool do_exc)
{
    ERL_NIF_TERM result;
    unsigned char* buf;
    size_t length;

    RET_ERROR(JsCopyString(obj, NULL, 0, &length));
    buf = enif_make_new_binary(env, length, &result);
    RET_ERROR(JsCopyString(obj, (char*) buf, length, NULL));

    *out = result;
    return true;
}


bool
js2erl_bool(ErlNifEnv* env, JsValueRef obj, ERL_NIF_TERM* out, bool do_exc)
{
    bool val;

    RET_ERROR(JsBooleanToBool(obj, &val));

    if(val) {
        *out = ATOM_true;
    } else {
        *out = ATOM_false;
    }

    return true;
}


JsErrorCode
js_to_prop_id(JsValueRef obj, JsPropertyIdRef* out)
{
    char* buf;
    size_t length;
    JsErrorCode err;

    err = JsCopyString(obj, NULL, 0, &length);
    if(err != JsNoError) {
        return err;
    }

    buf = enif_alloc(length * sizeof(char));
    err = JsCopyString(obj, buf, length, NULL);
    if(err != JsNoError) {
        goto done;
    }

    err = JsCreatePropertyId(buf, length, out);
    if(err != JsNoError) {
        goto done;
    }

done:
    enif_free(buf);
    return err;
}


bool
js2erl_object(ErlNifEnv* env, JsValueRef obj, ERL_NIF_TERM* out, bool do_exc)
{
    ERL_NIF_TERM props = enif_make_list(env, 0);
    ERL_NIF_TERM erl_key;
    ERL_NIF_TERM erl_val;
    ERL_NIF_TERM pair;

    JsValueRef prop_names;
    JsPropertyIdRef prop;
    JsValueRef idx;
    JsValueRef js_key;
    JsValueRef js_val;
    JsErrorCode err;

    int length;
    int i;

    err = JsGetOwnPropertyNames(obj, &prop_names);
    // Kinda hack, but if we hit out of memory then
    // this call fails with out of memory as well...
    if(err == JsErrorScriptException) {
        *out = ATOM_out_of_memory;
        return false;
    }
    if(err != JsNoError) {
        return _js2erl_error(env, err, out, do_exc);
    }

    RET_ERROR(JsCreatePropertyId("length", strlen("length"), &prop));
    RET_ERROR(JsGetProperty(prop_names, prop, &js_val));
    RET_ERROR(JsNumberToInt(js_val, &length));

    for(i = length - 1; i >= 0; i--) {
        RET_ERROR(JsIntToNumber(i, &idx));
        RET_ERROR(JsGetIndexedProperty(prop_names, idx, &js_key));
        RET_ERROR(js_to_prop_id(js_key, &prop));
        RET_ERROR(JsGetProperty(obj, prop, &js_val));

        if(!_js2erl(env, js_key, &erl_key, do_exc)) {
            return false;
        }

        if(!_js2erl(env, js_val, &erl_val, do_exc)) {
            return false;
        }

        pair = T2(env, erl_key, erl_val);
        props = enif_make_list_cell(env, pair, props);
    }

    *out = enif_make_tuple1(env, props);
    return true;
}


bool
js2erl_list(ErlNifEnv* env, JsValueRef obj, ERL_NIF_TERM* out, bool do_exc)
{
    ERL_NIF_TERM vals = enif_make_list(env, 0);
    ERL_NIF_TERM erl_val;

    JsPropertyIdRef prop;
    JsValueRef idx;
    JsValueRef js_val;

    int length;
    int i;

    RET_ERROR(JsCreatePropertyId("length", strlen("length"), &prop));
    RET_ERROR(JsGetProperty(obj, prop, &js_val));
    RET_ERROR(JsNumberToInt(js_val, &length));

    for(i = length - 1; i >= 0; i--) {
        RET_ERROR(JsIntToNumber(i, &idx));
        RET_ERROR(JsGetIndexedProperty(obj, idx, &js_val));

        if(!_js2erl(env, js_val, &erl_val, do_exc)) {
            return false;
        }

        vals = enif_make_list_cell(env, erl_val, vals);
    }

    *out = vals;
    return true;
}


bool
js2erl_any(ErlNifEnv* env, JsValueRef obj, ERL_NIF_TERM* out, bool do_exc)
{
    ERL_NIF_TERM result;

    JsValueRef str;

    unsigned char* buf;
    size_t length;

    RET_ERROR(JsConvertValueToString(obj, &str));
    RET_ERROR(JsCopyString(str, NULL, 0, &length));
    buf = enif_make_new_binary(env, length, &result);
    RET_ERROR(JsCopyString(str, (char*) buf, length, NULL));

    *out = result;
    return true;
}


bool
_js2erl(ErlNifEnv* env, JsValueRef obj, ERL_NIF_TERM* out, bool do_exc)
{
    JsValueType vt;

    RET_ERROR(JsGetValueType(obj, &vt));

    switch(vt) {
        case JsUndefined:
            *out = ATOM_undefined;
            return true;
        case JsNull:
            *out = ATOM_null;
            return true;
        case JsNumber:
            return js2erl_number(env, obj, out, do_exc);
        case JsString:
            return js2erl_string(env, obj, out, do_exc);
        case JsBoolean:
            return js2erl_bool(env, obj, out, do_exc);
        case JsObject:
        case JsError:
            return js2erl_object(env, obj, out, do_exc);
        case JsArray:
            return js2erl_list(env, obj, out, do_exc);
        case JsSymbol:
        case JsFunction:
        case JsArrayBuffer:
        case JsTypedArray:
        case JsDataView:
            return js2erl_any(env, obj, out, do_exc);
        default:
            *out = T2(env, ATOM_error, ATOM_invalid_value_type);
            return false;
    }
}

bool
_js2erl_error(ErlNifEnv* env, JsErrorCode error, ERL_NIF_TERM* out, bool do_exc)
{
    JsValueRef exc;
    JsErrorCode err;
    bool has_exc;

    if(!do_exc) {
        *out = T2(env, ATOM_error, js2erl_error_code(error));
        return false;
    }

    err = JsHasException(&has_exc);
    if(err != JsNoError) {
        *out = T2(env, ATOM_error, js2erl_error_code(err));
        return false;
    }

    if(has_exc || error == JsErrorScriptException) {
        err = JsGetAndClearException(&exc);
        if(err != JsNoError) {
            *out = T2(env, ATOM_error, js2erl_error_code(err));
            return false;
        }

        if(_js2erl(env, exc, out, false)) {
            *out = T2(env, ATOM_exception, *out);
        }
    } else {
        *out = T2(env, ATOM_error, js2erl_error_code(error));
    }

    return false;
}


bool
js2erl(ErlNifEnv* env, JsValueRef obj, ERL_NIF_TERM* out)
{
    return _js2erl(env, obj, out, true);
}


bool
js2erl_error(ErlNifEnv* env, JsErrorCode error, ERL_NIF_TERM* out)
{
    return _js2erl_error(env, error, out, true);
}


#define ERROR_MAP(ERROR, NAME) if(err == ERROR) return ATOM_##NAME
ERL_NIF_TERM
js2erl_error_code(JsErrorCode err)
{
    #include "errors.h"
    return ATOM_unknown;
}
#undef ERROR_MAP