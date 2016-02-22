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

#ifndef _RANDOM_H_
#define _RANDOM_H_

#ifndef RANDOM_DEV
#define RANDOM_DEV "/dev/urandom"
#endif

/*! \file random.h
    \brief Some helper functions to deal with pseudo random numbers
 */

/*! \brief Initializes pseudo random generator.
 *
 * \return used seed, falls back to 1, if /dev/some_random failed
 */
unsigned int random_init();

/*! \brief Computes a random index.
 *
 * \return random Integer r, with \f$0 <= r < n\f$
 */
int random_index(int n);

#endif
