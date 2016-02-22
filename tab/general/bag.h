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

#ifndef _BAG_H_
#define _BAG_H_

/*! \file bag.h
    \brief A bag (multiset) ADT.

    No boundary checks.
 */

/*! \brief Representation of the bag ADT. */
struct bag {
  int count; /*!< Number of elements in this bag. */ 
  int *array; /*!< internal */
};

/*! \brief Initializes this ADT.
    \param n max value the bag may contain
 */
void bag_init(struct bag *b, int n);

/*! \brief Empties a bag.
    \param n the same size the bag was initialized
 */
void bag_reset(struct bag *b, int n);

/*! \brief Makes a union of two bags.
    \param n the size the bags were initialized
 */
void bag_union(struct bag *a, struct bag *b, int n);

/*! \brief Makes a union of two bags and calls bag_delete on the 2nd argument.
    \param n the size the bags were initialized
 */
void bag_union_delete(struct bag *a, struct bag *b, int n);

/*! \brief Adds an element to the bag.
    \param i value
 */
void bag_add(struct bag *b, int i);

/*! \brief Removes an element from the bag.
    \param i value
 */
void bag_remove(struct bag *b, int i);

/*! \brief Tests if the bag contains the element.
    \param i value
    \return true, if i in b, else falls
 */
int bag_contains(struct bag *b, int i);

/*! \brief Frees the memory used by this ADT.
  
    Note, it doesn't free b.
 */
void bag_delete(struct bag *b);

#endif
