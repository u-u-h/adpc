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

#include "log.h"

#include <stdio.h>
#include <stdarg.h>

static int verb;

int log_get_verbosity()
{
  return verb;
}

void log_set_verbosity(int v)
{
  verb = v;
}

void log_printf(int v, const char *s, ...)
{
  va_list   vlist; 
  if (verb < v)
    return;
  va_start(vlist, s); 
  vfprintf(stdout, s, vlist);
  va_end(vlist);
}

int log_level(int v)
{
  if (verb < v)
    return 0;
  else
    return 1;
}
