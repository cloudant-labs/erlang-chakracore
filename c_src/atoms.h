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

ATOM_MAP(allow_script_interrupt);
ATOM_MAP(already_debugging_context);
ATOM_MAP(already_profiling_context);
ATOM_MAP(argument_not_object);
ATOM_MAP(bad_fpu_state);
ATOM_MAP(bad_serialized_script);
ATOM_MAP(cannot_disable_execution);
ATOM_MAP(cannot_serialize_debug_script);
ATOM_MAP(cannot_set_projection_enqueue_callback);
ATOM_MAP(cannot_start_projection);
ATOM_MAP(category_diag_error);
ATOM_MAP(category_engine);
ATOM_MAP(category_fatal);
ATOM_MAP(category_script);
ATOM_MAP(category_usage);
ATOM_MAP(diag_already_in_debug_mode);
ATOM_MAP(diag_invalid_handle);
ATOM_MAP(diag_not_at_break);
ATOM_MAP(diag_not_in_debug_mode);
ATOM_MAP(diag_object_not_found);
ATOM_MAP(diag_unable_to_perform_action);
ATOM_MAP(disable_background_work);
ATOM_MAP(disable_eval);
ATOM_MAP(disable_native_code_generation);
ATOM_MAP(enable_experimental_features);
ATOM_MAP(enable_idle_processing);
ATOM_MAP(error);
ATOM_MAP(exception);
ATOM_MAP(false);
ATOM_MAP(fatal);
ATOM_MAP(heap_enum_in_progress);
ATOM_MAP(idle_not_enabled);
ATOM_MAP(in_disabled_state);
ATOM_MAP(in_exception_state);
ATOM_MAP(in_object_before_collect_callback);
ATOM_MAP(in_profile_callback);
ATOM_MAP(in_thread_service_callback);
ATOM_MAP(interrupt);
ATOM_MAP(invalid_argument);
ATOM_MAP(invalid_argument_list);
ATOM_MAP(invalid_atom);
ATOM_MAP(invalid_binary);
ATOM_MAP(invalid_context);
ATOM_MAP(invalid_index);
ATOM_MAP(invalid_key);
ATOM_MAP(invalid_list);
ATOM_MAP(invalid_list_length);
ATOM_MAP(invalid_module_host_info_kind);
ATOM_MAP(invalid_number);
ATOM_MAP(invalid_pid);
ATOM_MAP(invalid_property);
ATOM_MAP(invalid_props);
ATOM_MAP(invalid_term);
ATOM_MAP(invalid_value_type);
ATOM_MAP(memory_limit);
ATOM_MAP(module_parsed);
ATOM_MAP(no_current_context);
ATOM_MAP(no_error);
ATOM_MAP(not_implemented);
ATOM_MAP(null);
ATOM_MAP(null_argument);
ATOM_MAP(object_not_inspectable);
ATOM_MAP(ok);
ATOM_MAP(out_of_memory);
ATOM_MAP(property_not_string);
ATOM_MAP(property_not_symbol);
ATOM_MAP(runtime_in_use);
ATOM_MAP(script_compile);
ATOM_MAP(script_eval_disabled);
ATOM_MAP(script_exception);
ATOM_MAP(script_terminated);
ATOM_MAP(source_url);
ATOM_MAP(true);
ATOM_MAP(undefined);
ATOM_MAP(undefined_function);
ATOM_MAP(unknown);
ATOM_MAP(wrong_runtime);
ATOM_MAP(wrong_thread);

#ifdef JsNoWeakRefRequired
ATOM_MAP(no_weak_ref_required);
#endif
#ifdef JsErrorPromisePending
ATOM_MAP(promise_pending);
#endif
