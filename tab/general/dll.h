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

#ifndef _DLL_H_
#define _DLL_H_

/*! \file dll.h
    \brief A double linked list ADT.

    It is a ring list.
 */

/*! \brief A double linked list element. */
struct list_head {
   struct list_head *next, 
                    *prev;
};

/*! \brief Initializes this ADT.
 
 next and prev pointer point at head
 */
void list_init(struct list_head *head);

/*! \brief Inserts new entry after specified head. */
void list_add(struct list_head *new, struct list_head *head);

/*! \brief Inserts a new entry before the specified head. */
void list_add_tail(struct list_head *new, struct list_head *head);

/*! \brief Removes entry from list and reinitializes it.
  
  (next = prev = 0)
  \return entry removed entry
 */
struct list_head* list_del(struct list_head *entry);

/*! \brief Removes entry from one list and insert it after another's head. */
void list_move(struct list_head *entry, struct list_head *head);

/*! \brief Removes entry from one list and insert it before another's head. */
void list_move_tail(struct list_head *entry, struct list_head *head);

/*! \brief Tests whether a list is empty.
    \return 0, if list empty, else != 0
 */
int list_empty(struct list_head *head);

/*! \brief Gets n-tes element */
struct list_head* list_element(struct list_head *head, int element);

/*! \brief Frees all memory used by this ADT. 

    Including the memory used by head.
 */
void list_free(struct list_head *head);

#endif
