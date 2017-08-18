///////////////////////////////////////////////////////////////////////////////
//      ______                 __     ____
//     / ____/   _____  ____  / /_   / __ \__  _____  __  _____
//    / __/ | | / / _ \/ __ \/ __/  / / / / / / / _ \/ / / / _ \
//   / /___ | |/ /  __/ / / / /_   / /_/ / /_/ /  __/ /_/ /  __/
//  /_____/ |___/\___/_/ /_/\__/   \___\_\__,_/\___/\__,_/\___/
//
//  Sean Wu
//  August 18, 2017
//  A specialized EventData structure for the MASH project
//  Fork of 'EventQueuer' package for R by Drew Schmidt (license below)
//
///////////////////////////////////////////////////////////////////////////////

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
    PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, EventData, OR
    PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "EventQueue.h"


EventQueue *EventQueue_create()
{
  EventQueue *eq;
  eq = malloc(sizeof(*eq));

  eq->start = NULL;
  eq->end = NULL;
  eq->len = 0;

  return eq;
}



void EventQueue_push(EventQueue *eq, SEXP EventData)
{
  Event *newE;
  newE = malloc(sizeof(*newE));

  newE->prev = NULL;
  newE->next = eq->start;
  newE->EventData = EventData;

  if (eq->start)
    eq->start->prev = newE;
  else
    eq->start = newE;

  if (!eq->end) eq->end = newE;
  eq->start = newE;
  eq->len++;
}



void EventQueue_pushback(EventQueue *eq, SEXP EventData)
{
  Event *newE;
  newE = malloc(sizeof(*newE));

  newE->prev = eq->end;
  newE->next = NULL;
  newE->EventData = EventData;

  if (eq->end)
    eq->end->next = newE;
  else
    eq->end = newE;

  if (!eq->start) eq->start = newE;
  eq->end = newE;
  eq->len++;
}



SEXP EventQueue_pop(EventQueue *eq)
{
  Event *tmp;

  if (eq->len == 0)
    return R_NilValue;

  Event *newE = eq->start;
  if (newE->next)
  {
    tmp = newE;
    newE = newE->next;
    newE->prev = NULL;

    newE = tmp;
  }

  eq->start = newE->next;
  if (eq->len == 1)
    eq->end = NULL;
  eq->len--;

  SEXP ret = newE->EventData;
  R_ReleaseObject(ret);
  free(newE);
  return ret;
}



SEXP EventQueue_popback(EventQueue *eq)
{
  Event *tmp;

  if (eq->len == 0)
    return R_NilValue;

  Event *newE = eq->end;

  if (newE->prev)
  {
    tmp = newE;
    newE = newE->prev;
    newE->next = NULL;

    newE = tmp;
  }

  eq->end = newE->prev;
  if (eq->len == 1)
    eq->start = NULL;
  eq->len--;

  SEXP ret = newE->EventData;
  R_ReleaseObject(ret);
  free(newE);
  return ret;
}



void EventQueue_reverse(EventQueue *eq)
{
  Event *tmp;
  const uint32_t len = eq->len;
  Event *newE;

  newE = eq->start;
  eq->start = eq->end;
  eq->end = newE;

  for (int i=0; i<len; i++)
  {
    tmp = newE->next;
    newE->next = newE->prev;
    newE->prev = tmp;

    newE = tmp;
  }
}



// split eq after k
int EventQueue_split(const uint32_t k, EventQueue *eq, EventQueue **eq2)
{
  if (eq->len < k) return -1;
  int i;
  *eq2 = EventQueue_create();
  Event *newE;

  if (k <= eq->len/2)
  {
    newE = eq->start;
    for (i=0; i<k; i++)
      newE = newE->next;
  }
  else
  {
    newE = eq->end;
    for (i=eq->len; i>k+1; i--)
      newE = newE->prev;
  }

  (*eq2)->len = eq->len - k;

  eq->end = newE->prev;
  eq->len = k;

  newE->prev = NULL;
  (*eq2)->start = newE;
  (*eq2)->end = eq->end;


  newE = eq->end;
  newE->next = NULL;

  return 0;
}



// this is very unsafe if you do dumb shit
int EventQueue_combine(EventQueue *eq, EventQueue *eq2)
{
  Event *newE;
  newE = eq->end;

  newE->next = eq2->start;
  eq2->start->prev = newE;

  eq->end = eq2->end;

  eq->len += eq2->len;

  eq2->start = NULL;
  eq2->end = NULL;
  eq2->len = 0;

  return 0;
}



void EventQueue_free(EventQueue *eq)
{
  Event *tmp;
  Event *newE = eq->start;

  while (newE)
  {
    if (newE->EventData != R_NilValue)
      R_ReleaseObject(newE->EventData);

    tmp = newE->next;
    free(newE);
    newE = tmp;
  }

  free(eq);
}
