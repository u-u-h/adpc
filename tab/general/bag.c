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
#include <string.h>
#include <assert.h>

#include "bag.h"

void bag_init(struct bag *b, int n)
{
  b->count = 0;
  b->array = calloc(n, sizeof(int));
}

void bag_reset(struct bag *b, int n)
{
  b->count = 0;
  memset(b->array, 0, n * sizeof(int));
}

void bag_union(struct bag *a, struct bag *b, int n)
{
  int i;

  if (!(b->count))
    return;
  a->count = 0;
  for (i=0; i<n; i++) {
    (a->array)[i] += (b->array)[i];
    a->count += (a->array)[i];
  }
}

void bag_union_delete(struct bag *a, struct bag *b, int n)
{
  bag_union(a, b, n);
  bag_delete(b);
}

void bag_add(struct bag *b, int i)
{
  ((b->array)[i])++;
  (b->count)++;
}

void bag_remove(struct bag *b, int i)
{
  assert((b->array)[i] != 0);
  ((b->array)[i])--;
  (b->count)--;
}
    
int bag_contains(struct bag *b, int i)
{
  return (b->array)[i];
}

void bag_delete(struct bag *b)
{
  free(b->array);
}

