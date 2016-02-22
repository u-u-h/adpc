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

#include "gray.h"

void bin_to_gray(struct gray_code *c)
{
  uint64 i, l;
  
  l = sizeof(c->gray_count) * 8;
  for (i=1; i < l; i++)
    if ((c->gray_count) & 1ull << i)
      (c->gray_count) ^= 1ull << (i - 1ull);
}

void gray_init(struct gray_code *c, uint64 b)
{
  c->bin_count = b;
  c->gray_count = b;
  if (b > 0)
    bin_to_gray(c);
}

int gray_inc(struct gray_code *c, int *state)
{
  uint64 i, l;

  l = sizeof(c->gray_count) * 8;
  for (i=0; i < l; i++)
    if ((c->bin_count) & 1ull << i)
      (c->bin_count) ^= 1ull << i;
    else {
      (c->bin_count) |= 1ull << i;
      break;
    }

  (c->gray_count) ^= 1 << i;
  
  if ((c->gray_count) & 1 << i)
    *state = 1;
  else
    *state = 0;
  return i;
}

int gray_compare(struct gray_code *c, struct gray_code *d)
{
  uint64 a = c->bin_count;
  uint64 b = d->bin_count;

  if (a < b)
    return -1;
  if (a > b)
    return 1;
  return 0;
}

void gray_print(struct gray_code *c)
{
  uint64 i, l;

  printf("%llu -> %llu\n", c->bin_count, c->gray_count);

  l = sizeof(c->gray_count) * 8;

  for (i=0; i < l; i++)
    printf("%d", c->bin_count & 1ull << i ? 1 : 0);
  printf("\n");
  for (i=0; i < l; i++)
    printf("%d", c->gray_count & 1ull << i ? 1 : 0);
  printf("\n");
}

void gray_mark_array(struct gray_code *c, char *array, uint64 length)
{
  uint64 i;

  for (i=0; i < length; i++)
    array[i] = c->gray_count & 1ull << i ? 0 : 1;

}

  
