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

#ifndef ERL_CHAKRA_QUEUE_H
#define ERL_CHAKRA_QUEUE_H

#include <stdbool.h>

#include "erl_nif.h"

typedef struct queue_t* queue_ptr;

queue_ptr queue_create();
void queue_destroy(queue_ptr queue);

void queue_interrupt(queue_ptr queue);
bool queue_has_item(queue_ptr queue);
bool queue_push(queue_ptr queue, ERL_NIF_TERM term);
bool queue_pop(queue_ptr queue, ErlNifEnv** env, ERL_NIF_TERM* term);

#endif // Included queue.h