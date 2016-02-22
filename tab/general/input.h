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

/*! \file input.h

    \brief Functions to create and delete the Asm_Nts-datastructure.

*/


#ifndef _INPUT_H_
#define _INPUT_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <tabulate.h>
#include <asm_typedefs.h>
#include <asm_runtime.h>


/*! \brief Convert the parser-dependencies given as "Nts"-representation to
    \brief "Asm_Nts"-representation.
    \param inputdata "Nts"-representation of the nonterminals
    \param result "Asm_Nts"-representation of the nonterminals (RESULT)
    \param Number Number of nonterminals (RESULT)
*/

void convert_input (Nts inputdata, Asm_Nts* result, int* Number);


/*! \brief Free all memory that was allocated for a "Asm_Nts"-representation
    \brief of the nonterminals.
    \param nonterms Nonterminals
    \param n Number of nonterminals
*/

void asm_nts_delete (Asm_Nts nonterms, int n);


#endif
