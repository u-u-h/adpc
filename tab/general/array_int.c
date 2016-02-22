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

#include <stdlib.h>
#include <assert.h>

#include "array_int.h"


void array_int_init(struct array_int *ai, int n)
{
  ai->count = 0;
  ai->elements = 0;
  ai->array = malloc(n * sizeof(int));
}

void array_int_reset(struct array_int *ai)
{
  ai->count = 0;
  ai->elements = 0;
}

void array_int_add(struct array_int *ai, int i)
{
  (ai->array)[ai->count++] = i;
  (ai->elements)++;
}

void array_int_remove(struct array_int *ai, int i)
{
  assert(ai->count > i);

  (ai->array)[i] = -1;
  (ai->elements)--;
}

int array_int_contains(struct array_int *ai, int i)
{
  assert(ai->count > i);

  return (ai->array)[i] != -1;
}

int array_int_get(struct array_int *ai, int i)
{
  assert((ai->array)[i] > -1);
  assert(ai->count > i);

  return (ai->array)[i];
}

void array_int_delete(struct array_int *ai)
{
  free(ai->array);
}
