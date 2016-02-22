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
#include "dll.h"

void list_init(struct list_head *head)
{
	head->next=head;
	head->prev=head;
}

void list_add(struct list_head *new, struct list_head *head)
{
	new->next=head->next;
	head->next=new;
	new->prev=head;
	new->next->prev=new;
}

void list_add_tail(struct list_head *new, struct list_head *head)
{
        new->prev=head->prev;
	head->prev=new;
        new->next=head;
	new->prev->next=new;
}

struct list_head* list_del(struct list_head *entry)
{
	entry->prev->next=entry->next;
	entry->next->prev=entry->prev;
	entry->next=entry->prev=0;
	return entry;	
}


void list_move(struct list_head *entry, struct list_head *head)
{
	list_del(entry);
	list_add(entry, head);
}

void list_move_tail(struct list_head *entry, struct list_head *head)
{
	list_del(entry);
	list_add_tail(entry, head);
}

int list_empty(struct list_head *head)
{
	if ((head->next==head) && (head->prev==head))
		return 1;
	else
		return 0;
}

struct list_head* list_element(struct list_head *head, int element)
{
	int i=0;
	struct list_head *next;
	next=head;
	while(((next=next->next)!=head) && (++i))
		if (i==element)
			return next;
	return 0;
}		


void list_free(struct list_head *head)
{
  struct list_head *itr;

  itr = head->next;
  while (itr != head) {
    itr = itr->next;
    free(itr->prev);
  }
  free(head);
}
