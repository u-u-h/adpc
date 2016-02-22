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

/*!\file hook.c
   
   \brief Implements the tabulate-function.

*/


#include <tabulate.h>
#include <taboptimal_in.h>
#include <adpc_grasp.h>

#include "tab/general/dep_graph.h"
#include "tab/general/poly_runtime.h"
#include "tab/general/log.h"


/*!\brief The startfunction for the C-code. This function is called by the
   \brief ADP-compiler (Haskell).
   \param arr The serialized array of the nonterminals and parser-dependencies
*/
int *tabulate(int *arr)
{
  int ap = 0; 
  enum action mode = arr[ap++];
  enum ret_action returnmode = arr[ap++];
  enum verbosity verb = arr[ap++];
  // max_additional_tables: maximal number of additional tables
  int max_additional_tables = arr[ap++];
  // needed_improvement: procentual value for neccessary constant
  // factor improvement
  // before an additional table (max_additional_tables) is added:
  int needed_improvement = arr[ap++];
  // Expected input length for constant factor improvement;
  // when expected_input_length is given, the constant factor improvement
  // exactly calculates the runtimes for the given length.
  // When expected_input_length == -1, constant factor improvement only
  // considers the factor of the highest exponent.
  int expected_input_length = arr[ap++];
  int number_of_nts = arr[ap];
  int size;
  int *return_array;

  struct dep_graph *g = NULL;

  Nts inputdata = deserializeGraph(arr+ap, &size);
  
  // always show results in RET_EXIT mode:
  if ((returnmode == RET_EXIT) && (verb == VERB_NONE)) verb = VERB_MORE;

  log_set_verbosity(verb);

  if (log_level(VERB_MOST)) {
    printNts(stdout, inputdata);
    printf("\n\nMode = %d, returnmode = %d, Verbosity = %d, "
        "max_additional_tables = %d, needed_improvement = %d\n", 
        mode, returnmode, verb, max_additional_tables, needed_improvement);
  }
  switch (mode) {
    case MODE_OPTIMAL:
      g = table_design_optimal(inputdata, verb-1);
      dep_graph_fix_annotation(inputdata, g);
      break;
    case MODE_APPROX:
      g = table_design_grasp(inputdata, verb);
      break;
    case MODE_PRINT:
      serialize_array(stdout, arr+ap, size);
      exit(0);
      break;
    case MODE_GOOD:
      fprintf(stderr, "mode good not implemented!");
      exit(1);
      break;
  }

  if (max_additional_tables) {
    if (!runtime_improve_constant_factors(g, max_additional_tables,
          needed_improvement, expected_input_length))
      log_printf(VERB_MORE,
          "Could improve config regarding constant factors.\n");
    else
      log_printf(VERB_MORE, "Couldnt improve constant factors.\n");
  }

  //return the numbers of the tabulated nonterminals as a zero-terminated arry:
  return_array = calloc(number_of_nts+1, sizeof(int));
  if (returnmode == RET_RESULT)
     if (g)
       dep_graph_tab_array(g, return_array);

  if (log_level(VERB_MOST)) {
    int i;
    printf("Returning: ");
    for (i=0; i <= number_of_nts; i++)
      printf("%d ", return_array[i]);
    printf("\n");
  }

  return return_array;
}
