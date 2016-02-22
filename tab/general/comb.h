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

#ifndef _COMB_H_
#define _COMB_H_


struct comb_handle {
  int pos, *max, *val, *_val, val_length, m, k, ready, *map;
};

void comb_init(struct comb_handle *handle, int max, int length);

int comb_next(struct comb_handle *handle);

void comb_print(struct comb_handle *handle);

struct comb_handle * comb_destroy(struct comb_handle *handle);

void comb_map_init(struct comb_handle *handle, int max, int *array, int length);

int comb_map_next(struct comb_handle *handle);


#endif
