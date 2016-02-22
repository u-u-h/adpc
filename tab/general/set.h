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

#ifndef _SET_H_
#define _SET_H_

/*! \file set.h
    \brief A set ADT
 */

/*! \brief set representation */
struct set {
  int size; /*!< maximal size of the set */
  int elements; /*!< the number of elements the set contains */
  int *field;
};

/*! \brief Initializes a set.
    \param size the maximal size of the set
 */
void set_new(struct set *s, int size);

void set_copy(struct set *s, struct set *t);

/*! \brief Allocate and initialize a new set.
    \param size the maximal size of the set
    \return new allocated set */
struct set *set_init(int size);

/*! \brief Tests if set contains the element.
    \param i set index
    \return true, if index in set, false else
 */
int set_contains(struct set *s, int i);

/*! \brief Adds an element to the set
    \param i index of element
 */
void set_add(struct set *s, int i);

/*! \brief Removes an element from the set
    \param i index of element
 */
void set_remove(struct set *s, int i);

/*! \brief Unites two sets
    \return union as new allocated set
 */
struct set *set_union(struct set *s, struct set *t);

/*! \brief Unites two sets in place
    
    size(s) == size(t)
    \param s first set, where the union gets saved */
void set_union_ip(struct set *s, struct set *t);

/*! \brief Intersects two sets in place

    size(s) == size(t)
    \param s first set, where the intersections gets saved */
void set_intersection_ip(struct set *s, struct set *t);

/*! \brief Pretty print the set */
void set_print(struct set *s);

/*! \brief Frees the complete set ADT
 
    Including s */
void set_free(struct set *s);

/*! \brief Frees the memory allocated by this ADT */
void set_delete(struct set *s);

#endif
