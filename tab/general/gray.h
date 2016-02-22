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

#ifndef _GRAY_H_
#define _GRAY_H_

/*! \file gray.h
    \brief Gray code ADT

    Can represent gray codes from 0 - 2^64-1.
 */

typedef long long uint64;

/*! \brief Gray code representation */
struct gray_code {
  uint64 gray_count; /*!< current gray code */
  uint64 bin_count; /*!< corresponding binary representation */
};

/*! \brief Initializes the ADT.
    \param b initial value binary encoded
 */
void gray_init(struct gray_code *c, uint64 b);

/*! \brief Increases the gray count by one.
    \param set to true, if digit changed to 1, else false
    \return the changed digit
 */
int gray_inc(struct gray_code *c, int *state);

/*! \brief Compares two gray codes.
    \return <0, 0, >0, if c < d, c == d, c>d
 */
int gray_compare(struct gray_code *c, struct gray_code *d);

/*! \brief Pretty the ADT.

    I.e. gray code and then binary code.
 */
void gray_print(struct gray_code *c);

/*! \brief Marks an array according to a specified gray code.
    \param length array length
 */
void gray_mark_array(struct gray_code *c, char *array, uint64 length);


#endif
