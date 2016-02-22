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

#include "stdlib.h"
#include "index_list.h"
#include "dll.h"

struct list_head** index_list_init(int n) {
  struct list_head **r, *head;
  r = calloc(n+1, sizeof(struct list_head*));
  head = malloc(sizeof(struct list_head));
  list_init(head);
  r[0] = head;
  return r;
}

void index_list_add(struct list_head **a, int n, struct list_head *h) {
  a[n] = h;
  list_add_tail(h, a[0]);
}

struct list_head *index_list_remove(struct list_head **a, int n) {
  struct list_head *r;
  r = a[n];
  a[n] = NULL;
  list_del(r);
  return r;
}

void index_list_free(struct list_head **a) {
  struct list_head *head, *itr, *t;
  head = a[0];
  itr = head->next;
  while (itr != head) {
    t = itr;
    itr = itr->next;
    free(t);
  }
  free(head);
  free(a);
}

