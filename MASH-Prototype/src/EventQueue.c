///////////////////////////////////////////////////////////////////////////
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
  Event *l;
  l = malloc(sizeof(*l));

  l->prev = NULL;
  l->next = eq->start;
  l->EventData = EventData;

  if (eq->start)
    eq->start->prev = l;
  else
    eq->start = l;

  if (!eq->end) eq->end = l;
  eq->start = l;
  eq->len++;
}



void EventQueue_pushback(EventQueue *eq, SEXP EventData)
{
  Event *l;
  l = malloc(sizeof(*l));

  l->prev = eq->end;
  l->next = NULL;
  l->EventData = EventData;

  if (eq->end)
    eq->end->next = l;
  else
    eq->end = l;

  if (!eq->start) eq->start = l;
  eq->end = l;
  eq->len++;
}



SEXP EventQueue_pop(EventQueue *eq)
{
  Event *tmp;

  if (eq->len == 0)
    return R_NilValue;

  Event *l = eq->start;
  if (l->next)
  {
    tmp = l;
    l = l->next;
    l->prev = NULL;

    l = tmp;
  }

  eq->start = l->next;
  if (eq->len == 1)
    eq->end = NULL;
  eq->len--;

  SEXP ret = l->EventData;
  R_ReleaseObject(ret);
  free(l);
  return ret;
}



SEXP EventQueue_popback(EventQueue *eq)
{
  Event *tmp;

  if (eq->len == 0)
    return R_NilValue;

  Event *l = eq->end;

  if (l->prev)
  {
    tmp = l;
    l = l->prev;
    l->next = NULL;

    l = tmp;
  }

  eq->end = l->prev;
  if (eq->len == 1)
    eq->start = NULL;
  eq->len--;

  SEXP ret = l->EventData;
  R_ReleaseObject(ret);
  free(l);
  return ret;
}



void EventQueue_reverse(EventQueue *eq)
{
  Event *tmp;
  const uint32_t len = eq->len;
  Event *l;

  l = eq->start;
  eq->start = eq->end;
  eq->end = l;

  for (int i=0; i<len; i++)
  {
    tmp = l->next;
    l->next = l->prev;
    l->prev = tmp;

    l = tmp;
  }
}



// split eq after k
int EventQueue_split(const uint32_t k, EventQueue *eq, EventQueue **eq2)
{
  if (eq->len < k) return -1;
  int i;
  *eq2 = EventQueue_create();
  Event *l;

  if (k <= eq->len/2)
  {
    l = eq->start;
    for (i=0; i<k; i++)
      l = l->next;
  }
  else
  {
    l = eq->end;
    for (i=eq->len; i>k+1; i--)
      l = l->prev;
  }

  (*eq2)->len = eq->len - k;

  eq->end = l->prev;
  eq->len = k;

  l->prev = NULL;
  (*eq2)->start = l;
  (*eq2)->end = eq->end;


  l = eq->end;
  l->next = NULL;

  return 0;
}



// this is very unsafe if you do dumb shit
int EventQueue_combine(EventQueue *eq, EventQueue *eq2)
{
  Event *l;
  l = eq->end;

  l->next = eq2->start;
  eq2->start->prev = l;

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
  Event *l = eq->start;

  while (l)
  {
    if (l->EventData != R_NilValue)
      R_ReleaseObject(l->EventData);

    tmp = l->next;
    free(l);
    l = tmp;
  }

  free(eq);
}
