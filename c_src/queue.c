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

#include <assert.h>
#include <stdio.h>

#include "queue.h"


struct _qitem_t
{
    struct _qitem_t* next;
    ErlNifEnv* env;
    ERL_NIF_TERM term;
};

typedef struct _qitem_t qitem_t;

struct queue_t
{
    ErlNifMutex* lock;
    ErlNifCond* cond;
    qitem_t* head;
    qitem_t* tail;
    size_t length;
    bool interrupt;
};


queue_ptr
queue_create()
{
    queue_ptr ret;

    ret = (queue_ptr) enif_alloc(sizeof(struct queue_t));
    if(ret == NULL) {
        goto error;
    }

    ret->lock = NULL;
    ret->cond = NULL;
    ret->head = NULL;
    ret->tail = NULL;
    ret->length = 0;
    ret->interrupt = false;

    ret->lock = enif_mutex_create("queue_lock");
    if(ret->lock == NULL) {
        goto error;
    }

    ret->cond = enif_cond_create("queue_cond");
    if(ret->cond == NULL) {
        goto error;
    }

    return ret;

error:
    if(ret != NULL) {
        if(ret->lock != NULL) {
            enif_mutex_destroy(ret->lock);
        }

        if(ret->cond != NULL) {
            enif_cond_destroy(ret->cond);
        }

        enif_free(ret);
    }

    return NULL;
}


void
queue_destroy(queue_ptr queue)
{
    ErlNifMutex* lock;
    ErlNifCond* cond;
    qitem_t* head;
    qitem_t* next;
    int length;

    enif_mutex_lock(queue->lock);
    lock = queue->lock;
    cond = queue->cond;
    head = queue->head;
    length = queue->length;

    queue->lock = NULL;
    queue->cond = NULL;
    queue->head = NULL;
    queue->tail = NULL;
    queue->length = -1;
    enif_mutex_unlock(lock);

    while(head != NULL) {
        next = head->next;
        enif_free_env(head->env);
        enif_free(head);
        head = next;
    }

    enif_cond_destroy(cond);
    enif_mutex_destroy(lock);
    enif_free(queue);
}


void
queue_interrupt(queue_ptr queue)
{
    enif_mutex_lock(queue->lock);
    queue->interrupt = true;
    enif_cond_signal(queue->cond);
    enif_mutex_unlock(queue->lock);
}


bool
queue_has_item(queue_ptr queue)
{
    bool ret;

    enif_mutex_lock(queue->lock);
    ret = (queue->head != NULL);
    enif_mutex_unlock(queue->lock);

    return ret;
}


bool
queue_push(queue_ptr queue, ERL_NIF_TERM term)
{
    qitem_t* item = (qitem_t*) enif_alloc(sizeof(qitem_t));
    if(item == NULL) {
        return false;
    }

    item->next = NULL;
    item->env = enif_alloc_env();
    item->term = enif_make_copy(item->env, term);

    enif_mutex_lock(queue->lock);

    if(queue->tail != NULL)
    {
        queue->tail->next = item;
    }

    queue->tail = item;

    if(queue->head == NULL)
    {
        queue->head = queue->tail;
    }

    queue->length += 1;

    enif_cond_signal(queue->cond);
    enif_mutex_unlock(queue->lock);

    return true;
}


bool
queue_pop(queue_ptr queue, ErlNifEnv** env, ERL_NIF_TERM* term)
{
    qitem_t* item;

    enif_mutex_lock(queue->lock);

    // Wait for an item to become available.
    while(queue->head == NULL && !queue->interrupt)
    {
        enif_cond_wait(queue->cond, queue->lock);
    }

    if(queue->head == NULL && queue->interrupt) {
        queue->interrupt = false;
        enif_mutex_unlock(queue->lock);
        return false;
    }

    queue->interrupt = false;

    // Woke up because queue->head != NULL
    // Remove the entry and return the payload.

    item = queue->head;
    queue->head = queue->head->next;

    if(queue->head == NULL)
    {
        queue->tail = NULL;
    }

    queue->length -= 1;

    enif_mutex_unlock(queue->lock);

    *env = item->env;
    *term = item->term;
    enif_free(item);

    return true;
}
