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
#include <unistd.h>

#include "tab/general/adpc_deserialize.h"

#include "tab/general/random.h"

int *tabulate(int *arr);

static void help(char **argv)
{
    printf("%s [-mainh] filename.i\n", argv[0]);
}

// C-Einstieg um den table design Teil unabhaengig von dem adpc
// zu testen. Eine serialisierte Eingabe (.i) wird entsprechend
// deserialisiert und im weiteren wird der selbe Einstieg (hook.c)
// verwendet, den auch der adpc verwendet.
int main(int argc, char **argv)
{
  int *array;
  //int r;
  char c;
  int mode = 0,
      addt = 0,
      imp = 0,
      n = -1;
  char *filename;
 

  while ((c = getopt(argc, argv, "m:a:i:n:h")) != -1)
    switch (c) {
      case 'm' : mode = atoi(optarg); break;
      case 'a' : addt = atoi(optarg); break;
      case 'i' : imp = atoi(optarg); break;
      case 'n' : n = atoi(optarg); break;
      
      case 'h' : 
      case '?' :
      default  : help(argv);
                 return 1;
                 break;
    }
  if (optind < argc)
      filename = argv[optind];
  else {
    help(argv);
    return 2;
  }

  //unsigned int seed = random_init();
  //printf("Random seed: %d\n", seed);
  
  array = adpc_readarray(filename, 6);

  int ap = 0; 
  //enum action mode = arr[ap++];
//  array[ap++] = MODE_OPTIMAL;
  if (mode)
    array[ap++] = MODE_APPROX;
  else
    array[ap++] = MODE_OPTIMAL;
  //enum ret_action returnmode = arr[ap++];
  array[ap++] = RET_EXIT;
  //enum verbosity verb = arr[ap++];
  array[ap++] = VERB_MOST;
  // max_additional_tables: maximal number of additional tables
  //int max_additional_tables = arr[ap++];
//  array[ap++] = 2;
  array[ap++] = addt;
  // needed_improvement: procentual value for neccessary constant
  // factor improvement
  // before an additional table (max_additional_tables) is added:
  //int needed_improvement = arr[ap++];
  array[ap++] = imp;
  // Expected input length for constant factor improvement;
  // when expected_input_length is given, the constant factor improvement
  // exactly calculates the runtimes for the given length.
  // When expected_input_length == -1, constant factor improvement only
  // considers the factor of the highest exponent.
  //int expected_input_length = arr[ap++];
  array[ap++] = n;
  
  tabulate(array);


  return 0;
}
