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

#include "random.h"
#include "log.h"

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

unsigned int random_init()
{
  FILE *f;
  unsigned int z;
  LOG("Initialising pseudo random generator\n");
  if (!(f=fopen(RANDOM_DEV,"r"))) {
    perror(NULL);
    return 1;
  }
  if (!fread(&z, sizeof(unsigned int), 1, f))
    return 1;
  fclose(f);
  LOG("Got seed: %u\n", z);
  srand(z);
  return z;
}

int random_index(int n)
{
  int r;
  /* from de.comp.lang.c FAQ - uses higher bits */
  r = (int)((double)rand() / ((double)RAND_MAX + 1) * n);


  return r;
}
