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

/*! \file input.c

    \brief Functions to create and delete the Asm_Nts-datastructure.

*/


#include <input.h>

    
void convert_input (Nts inputdata, Asm_Nts* result, int* Number)

  { // Allocate memory for the nonterminal-records
    int default_nonterminal_number = 100;
    Asm_Nts outputdata
      = malloc(default_nonterminal_number*sizeof(struct asm_nts));
    int Num = 0, count = 1;
    Asm_Deps OutputDep;
    Deps InputDep;
    
    // convert list -> array
    while (inputdata)
      { outputdata[Num].nt      = inputdata->nt - 1;
        outputdata[Num].tab     = inputdata->tab;
        outputdata[Num].runtime = poly2asm(inputdata->runtime);
        outputdata[Num].space   = poly2asm(inputdata->tabulationType);
        outputdata[Num].usage   = -1;
        // convert dependencies
        outputdata[Num].deps = NULL;
        InputDep = inputdata->deps;
        while (InputDep)
          { OutputDep = outputdata[Num].deps;
            outputdata[Num].deps = malloc(sizeof(struct asm_deps));
            outputdata[Num].deps->nt     = InputDep->nt - 1;
            outputdata[Num].deps->usages = poly2asm(InputDep->usages);
            outputdata[Num].deps->next   = OutputDep;
            InputDep = InputDep->next;
          }
        Num++;
        // Do we have to allocate more memory ?
        if (Num % default_nonterminal_number == 0)
          { count++;
            outputdata = realloc(outputdata, count*default_nonterminal_number*
                                             sizeof(struct asm_nts));
          }
        inputdata = inputdata->next;
      }
    
    // Set array-size to the exact size and return results
    *result = realloc(outputdata, Num*sizeof(struct asm_nts));
    *Number = Num;
  }
    

void asm_nts_delete (Asm_Nts nonterms, int n)

  { int i;
    Asm_Deps deps;
    for (i = 0; i < n; i++)
      while (nonterms[i].deps)
        { deps = nonterms[i].deps;
          nonterms[i].deps = nonterms[i].deps->next;
          free(deps);
        }
    free(nonterms);
  }
