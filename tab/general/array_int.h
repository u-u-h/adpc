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

#ifndef _ARRAY_INT_H_
#define _ARRAY_INT_H_

/*! \file array_int.h
  \brief An integer array ADT.
 
 You have random access. New Elements get inserted at the end.
 Removing Elements doesn't shrink the array.
 Without boundary checks. For values >= 0. */

/*! \brief Representation of the integer array ADT. */
struct array_int {
  int count; /*!< Internal, current position, where to put the next value. */
  int elements; /*!< Number of inserted non-deleted elements */
  int *array; /*!< Internal */
};

/*! \brief Initializes the ADT.
    \param ai pointer to allocated struct
    \param n maximum number of possible added elements
 */
void array_int_init(struct array_int *ai, int n);

/*! \brief Resets the existing instance to the initial state. */
void array_int_reset(struct array_int *ai);

/*! \brief Adds a value at the end of the array.
    \param i value of new element
 */
void array_int_add(struct array_int *ai, int i);

/*! \brief Removes an element at an index.
    \param i index
 */
void array_int_remove(struct array_int *ai, int i);

/*! \brief Tests if the array contains an element at a specified index.
    \param i index
    \return true, if contains element at index i, else false
 */
int array_int_contains(struct array_int *ai, int i);

/*! \brief Gets the value at a specified index.
    \param i index
    \return value
 */
int array_int_get(struct array_int *ai, int i);

/*! \brief Frees all memory used by this ADT. */
void array_int_delete(struct array_int *ai);

#endif
