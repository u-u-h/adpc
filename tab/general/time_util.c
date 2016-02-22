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

#include <stdio.h>
#include <sys/time.h>
#include "time_util.h"

void tval_minus(struct timeval *result,
    struct timeval *tv1, struct timeval *tv2)
{
  long usec;
  result->tv_sec = tv2->tv_sec - tv1->tv_sec;
  usec = tv2->tv_usec - tv1->tv_usec;
  if (usec < 0) {
    (result->tv_sec)--;
    result->tv_usec = usec + 1000000;
  } else
    result->tv_usec = usec;
}

void tval_print(struct timeval *tv)
{
  printf("%ld sec, %ld usec", tv->tv_sec, tv->tv_usec);
}


#ifdef WINDOWS

int gettimeofday(struct timeval *tp, void *tzp)
{
  time_t curTime = time(NULL);
  tp->tv_sec = (double) curTime;
  tp->tv_usec = 0;
  if (!tzp)
    return 0;
  return -1;
}

#endif
