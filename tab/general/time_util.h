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

#ifndef _TIME_UTIL_H_
#define _TIME_UTIL_H_

/*! \file time_util.h
    \brief Some functions to help with struct timeval */

/*! \brief Substracts two timevals
    \return result = tv2 - tv1
 */
void tval_minus(struct timeval *result,
    struct timeval *tv1, struct timeval *tv2);

/*! \brief Pretty print the timeval */
void tval_print(struct timeval *tv);


#ifdef WINDOWS

/** On Microsoft Windows operating systems, the gettimeofday function is
    usually not available. For this purpose we implement a very simple one,
    which precision is one second. */

/*! \brief A very simple implementation for Microsoft Windows.

    This function is designed for the use in this software only and should not
    be used in general */
int gettimeofday(struct timeval *tp, void *tzp);

#endif

#endif
