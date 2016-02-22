/*
------------------------------------------------------------------------------
-- The ADP Compiler 
-- Copyright (C) 2001-2008 Peter Steffen, Christian Lang, Marco Ruether, 
--                         Georg Sauthoff, Stefanie Schirmer
--
-- Send comments/bug reports to: P.Steffen <psteffen@techfak.uni-bielefeld.de>.
-- Updates: http://bibiserv.techfak.uni-bielefeld.de/adp/adpcomp.html
------------------------------------------------------------------------------

This file is part of ADPC (The ADP Compiler).

ADPC is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

ADPC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with ADPC.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <assert.h>

#include "set.h"


#define INT_BITS (sizeof(int) * 8)

int set_int_count(int size) {
  return (size / INT_BITS + (size % INT_BITS > 0 ? 1 : 0) );
}

void set_new(struct set *s, int size)
{
   s->size = size;
   s->elements = 0;
   s->field = calloc(set_int_count(size), sizeof(int));
}

void set_copy(struct set *s, struct set *t)
{
  assert(s->size == t->size);
  s->elements = t->elements;
  memcpy(s->field, t->field, set_int_count(s->size) * sizeof(int));
}

struct set *set_init(int size) {
  struct set *s = malloc(sizeof(struct set));
  s->size = size;
  s->elements = 0;
  s->field = calloc(set_int_count(size), sizeof(int));
  return s;
}

int set_contains(struct set *s, int i) {
  int *f = s->field;
  int a, b;

  assert(i < s->size);

  a = i / INT_BITS;
  b = i % INT_BITS;

  f += a;
  return ((*f) & 1 << b);
}

void set_add(struct set *s, int i) {
  int *f = s->field;
  int a, b;

  assert(!set_contains(s, i));

  (s->elements)++;
  a = i / INT_BITS;
  b = i % INT_BITS;

  f += a;
  *f |= 1 << b;
}

void set_remove(struct set *s, int i) {
  int *f = s->field;
  int a, b;

  (s->elements)--;
  a = i / INT_BITS;
  b = i % INT_BITS;

  f += a;
  *f ^= 1 << b;
}

int set_count(int *x, int a) {
  int i, j, r=0;
  for (i=0; i<a; i++)
    for (j=0; j<INT_BITS; j++)
      if (x[i] & 1 << j)
        r++;
  return r;
}

struct set *set_union(struct set *s, struct set *t) {
  int a, b, min, max, max_size;
  int *r, *x, *y, *f;
  int i;
  struct set *u;

  a = set_int_count(s->size);
  b = set_int_count(t->size);

  max = a > b ? a : b;
  min = a < b ? a : b;
  
  max_size = s->size > t->size ? s->size : t->size;

  u = set_init(max_size);

  r = u->field;
  x = s->field;
  y = t->field;

  for (i=0; i<min; i++)
    r[i] = x[i] | y[i];
  if (a == b) {
    u->elements = set_count(r, a);
    return u;
  }
  
  if (a < b)
    f = y;
  else
    f = x;

  for (; i < max; i++)
    r[i] = f[i];
  u->elements = set_count(r, a);
  return u;
}

void set_union_ip(struct set *s, struct set *t) {
  int a;
  int *x, *y;
  int i;

  assert(s->size == t->size);

  a = set_int_count(s->size);
  
  x = s->field;
  y = t->field;

  for (i=0; i<a; i++)
    x[i] |= y[i];
  s->elements = set_count(x, a);
}

void set_intersection_ip(struct set *s, struct set *t) {
  int a;
  int *x, *y;
  int i;

  assert(s->size == t->size);

  a = set_int_count(s->size);
  
  x = s->field;
  y = t->field;

  for (i=0; i<a; i++)
    x[i] &= y[i];
  s->elements = set_count(x, a);
}

void set_print(struct set *s) {
  int size = set_int_count(s->size);
  int i, j;
  for (i=0; i<size; i++)
    for (j=0; j<INT_BITS; j++)
      if ((s->field[i] & 1 << j))
        printf("%d ", (INT_BITS * i + j));
  printf("\n");
}

void set_free(struct set *s)
{
  free(s->field);
  free(s);
}

void set_delete(struct set *s)
{
  free(s->field);
}

