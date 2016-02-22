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

#include "comb.h"

#include <stdio.h>
#include <stdlib.h>


void comb_init(struct comb_handle *handle, int k, int length)
{
  handle->max = malloc(k * sizeof(int));
  handle->_val = malloc(k * sizeof(int));
  handle->val = handle->_val;
  handle->val_length=0;
  handle->ready=1;
  if (k > length)
    handle->k = length;
  else
    handle->k = k;
  handle->m = length;
  handle->map = NULL;
}

void comb_clean(struct comb_handle *h)
{
  int i, j;
  h->pos = h->val_length - 1;
  for (i=0; i < h->val_length; i++)
    (h->_val)[i] = i;
  j = h->m-1;
  for (i=h->val_length-1; i >= 0; i--)
    (h->max)[i] = j--;
}

int fixed_next(struct comb_handle *h)
{
  int j;
  for (;;) {
    if ((h->_val)[h->pos] < (h->max)[h->pos]) {
      ((h->_val)[h->pos])++;
      return 0;
    } else {
      if (h->pos > 0)
        (h->pos)--;
      else
        return 1;
      if ((h->_val)[h->pos] < (h->max)[h->pos]) {
        ((h->_val)[h->pos])++;
        for (j = h->pos+1; j < h->val_length; j++) {
          (h->_val)[j] = (h->_val)[j-1]+1;
          (h->pos)++;
        }
        return 0;
      }
    }
  }
}

int comb_next(struct comb_handle *h)
{
  int r;

  for (;;) {
    if (h->ready) {
      if (h->val_length < h->k) {
        (h->val_length)++;
        h->ready = 0;
        comb_clean(h);
        return 0;
      } else
        return 1;
    } else {
      r = fixed_next(h);
      if (r)
        h->ready = 1;
      else
        return r;
    }
  }
}

struct comb_handle * comb_destroy(struct comb_handle *handle)
{
  free(handle->max);
  if (handle->val == handle->_val)
    free(handle->val);
  else {
    free(handle->val);
    free(handle->_val);
  }
  if (handle->map)
    free(handle->map);
  return handle;
}

void comb_print(struct comb_handle *handle)
{
  int i;
  for (i = 0; i < handle->val_length; i++)
    printf("%d ", (handle->val)[i]);
  printf("\n");
}

void comb_map_init(struct comb_handle *handle, int max, int *array, int length)
{
  int i, j;

  int *map = malloc(length * sizeof(int));
  
  j = 0;
  for (i = 0; i < length; i++)
    if (!(array[i]))
      map[j++] = i;

  comb_init(handle, max, j);
  handle->val = malloc(max * sizeof(int));
  handle->map = map;
}

int comb_map_next(struct comb_handle *handle)
{
  int i;
  int r = comb_next(handle);
  if (r)
    return r;
  for (i = 0; i < handle->val_length; i++)
    handle->val[i] = (handle->map)[(handle->_val)[i]];
  return r;
}
