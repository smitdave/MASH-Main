///////////////////////////////////////////////////////////////////////////
//      ______                 __     ____
//     / ____/   _____  ____  / /_   / __ \__  _____  __  _____
//    / __/ | | / / _ \/ __ \/ __/  / / / / / / / _ \/ / / / _ \
//   / /___ | |/ /  __/ / / / /_   / /_/ / /_/ /  __/ /_/ /  __/
//  /_____/ |___/\___/_/ /_/\__/   \___\_\__,_/\___/\__,_/\___/
//
//  Sean Wu
//  August 18, 2017
//  A specialized data structure for the MASH project
//  Fork of 'EventQueuer' package for R by Drew Schmidt (license below)
//
///////////////////////////////////////////////////////////////////////////

/*  Copyright (c) 2015, Schmidt
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
    TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
    CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
    EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
    PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
    PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

// name changes:
// deque_t: EventQueue
// list_t: Event
// data: EventData

#ifndef _EVENTQUEUE_H_
#define _EVENTQUEUE_H_

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#include <stdlib.h>
#include <stdint.h>

typedef struct EventQueue{
  struct Event *start;
  struct Event *end;
  uint32_t len;
} EventQueue;

typedef struct Event{
  struct Event *next;
  struct Event *prev;
  SEXP EventData;
} Event;

#define PRINTORDER_FORWARD_EVENTQUEUE 1
#define PRINTORDER_FORWARD_EVENTQUEUE 2

#define PEEKER_HEAD_EVENTQUEUE 1
#define PEEKER_TAIL_EVENTQUEUE 2

#define PRINT_FEW_EVENTQUEUE 1
#define PRINT_ALL_EVENTQUEUE 2


// External pointer shorthand
#define CHECKPTR(ptr) if(ptr==NULL)error("queue/stack/EventQueue is invalid: pointer is NULL")

#define newRptr(ptr,Rptr,fin) PROTECT(Rptr = R_MakeExternalPtr(ptr, R_NilValue, R_NilValue));R_RegisterCFinalizerEx(Rptr, fin, TRUE)
#define getRptr(ptr) R_ExternalPtrAddr(ptr);

// Misc R shorthand
#define CHARPT(x,i)	((char*)CHAR(STRING_ELT(x,i)))
#define INT(x) INTEGER(x)[0]


EventQueue *EventQueue_create();
void EventQueue_push(EventQueue *eq, SEXP data);
void EventQueue_pushback(EventQueue *eq, SEXP data);
SEXP EventQueue_pop(EventQueue *eq);
SEXP EventQueue_popback(EventQueue *eq);
void EventQueue_reverse(EventQueue *eq);
int EventQueue_split(const uint32_t k, EventQueue *eq, EventQueue **eq2);
int EventQueue_combine(EventQueue *eq, EventQueue *eq2);
void EventQueue_free(EventQueue *eq);
void EventQueue_print(EventQueue *eq);


#endif
