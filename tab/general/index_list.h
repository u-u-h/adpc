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

#ifndef _INDEX_LIST_H_
#define _INDEX_LIST_H_

/*! \file index_list.h
    \brief A random access list ADT.

    You can traverse it like the double linked list in the order, the elements
    got added but access them by index directly.
 */

#include "dll.h"

/*! \brief Initializes the ADT.
    \param n max element count, at 0 the list_head, 0 < elements < n+1
    \return allocated ADT
 */
struct list_head** index_list_init(int n);

/*! \brief Adds a new element at index n */
void index_list_add(struct list_head **a, int n, struct list_head *h);

/*! \brief Removes an element from index n */
struct list_head *index_list_remove(struct list_head **a, int n);

/*! \brief Frees the complete ADT

    Including a.
 */
void index_list_free(struct list_head **a);

#endif
