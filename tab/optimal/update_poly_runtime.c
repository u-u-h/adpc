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

/*! \file update_poly_runtime.c

    \brief Auxiliary program to update the polynomial runtime of all
    \brief table configurations in a result file.

    It recomputes the polynomial runtime of all asymptotically optimal
    table configurations that are listed in a result-file and recreates
    the list of all polynomial optimal table configuartions.

*/


#include <stdio.h>
#include <string.h>

#include <tabulate.h>
#include <asm_typedefs.h>
#include <asm_runtime.h>
#include <functions.h>
#include <graph.h>
#include <poly_type.h>
#include <poly_runtime.h>

/*! \brief The string that indicates the begin of the list of the 
    \brief asymptotically optimal table configurations in the
    \brief result-file. 
*/
#define KEY_WORD "Optimal table configurations"


int main (int argc, char** argv)

  { if (argc != 4)
      { fprintf(stderr, "\nRecomputes the polynomial runtime of all "
                        "asymptotically optimal table\nconfigurations "
                        "that are listed in a result-file.\n"
                        "\nUsage:  %s <adp-grammar> <result_old> "
                        "<result_new>\n\n"
                        "The program reads the grammar from <adp-grammar> "
                        "and the old results from\n<result_old>. It "
                        "writes the updated results to <result_new>.\n\n",
                        argv[0]);
         return 0;
      }
      
    // Get parameters    
    char* grammar_filename = argv[1];
    char* result_filename = argv[2];
    char* output_filename = argv[3];
    
    // Read, deserialize and convert the adp-grammar
    int* serialized_arr;
    int arr_size;
    FILE* grammar_file = fopen(grammar_filename, "r");
    if (!grammar_file)
      { fprintf(stderr, "\nError: Cannot open file \'%s\'.\n\n",
                         grammar_filename);
        return 1;
      }
    read_array(grammar_file,&serialized_arr,&arr_size);
    if (!serialized_arr)
      { fprintf(stderr, "\nError: Cannot read data from \'%s\': "
                        "invalid file-format.\n\n",
                        grammar_filename);
        return 1;
      }
    fclose(grammar_file);
    Asm_Nts nonterms;
    Number_Info number;
    Nts inputdata = deserializeGraph(serialized_arr,&arr_size);
    convert_input(inputdata, &nonterms, &(number.total));
    // Tread all nonterminals as optimization-nontetminals
    number.opt = number.total;
    asm_get_usages(nonterms, number.total, 0);  // 0 <-> axiom
    struct dep_graph* nonterms_graph;
    nonterms_graph = (struct dep_graph*) graph_init(50, 50, 10,
                                                    sizeof(struct dep_graph),
                                                    sizeof(struct nt_vertex),
                                                    sizeof(struct dep_edge),
                                                    NULL);
    struct graph* graph = &(nonterms_graph->graph);
    nts_to_graph(inputdata, graph);    
    
    // Open the result- and the output-file
    FILE* result_file = fopen(result_filename, "r");
    if (!result_file)
      { fprintf(stderr, "\nError: Cannot open file \'%s\'.\n\n",
                         grammar_filename);
        return 1;
      }
    FILE* output_file = fopen(output_filename, "w");
      
    // Update the polynomial runtimes
    int key_word_len = strlen(KEY_WORD);
    int cont = 1;
    unsigned int OptCount;
    char* line = malloc(10000);
    char* aux = malloc(100);
    // Search for the keyword in the result-file. Copy all lines before the
    // keyword to the output-file. Read the number of asymptotically optimal
    // table configurations.
    while (!feof(result_file) && cont)
      { fscanf(result_file, "%10000[^\n]", line);
        fscanf(result_file, "%10000[\n]", aux);
        fprintf(output_file, "%s%s", line, aux);
        if (!strncmp(line, KEY_WORD, key_word_len))
          { aux = strchr(line, (int) ':');
            if (!aux)
              { fprintf(stderr, "\nError: Invalid file format of file "
                                "\'%s\'.\n\n", result_filename);
                fclose(result_file);
                fclose(output_file);
                remove(output_filename);
                return 1;
              }
            sscanf(aux+1, "%u", &OptCount);
            cont = 0;
          }
      }
    if (cont)
      { fprintf(stderr, "\nError: Invalid file format of file "
                        "\'%s\'.\n\n", result_filename);
        return 1;
      }  
    
    // Read the asymptotically optimal table configurations
    TableDesign_List Tables = NULL, CurTable = NULL;
    struct nt_vertex *v;
    int i,j,q,x;
    int OptCard, Card; // Dummies
    char* GrayCount = malloc(number.opt);
    char* line_base = line;
    i = 0;
    while ((i < OptCount) && !feof(result_file))
      { line = line_base;
        // Initial: empty table configuration
        for (j = 0; j < number.opt; j++)
          { GrayCount[j] = 1;
            v = (struct nt_vertex*) graph_vertex(graph,j+1);
            v->tabulated = 0;
          }
        fscanf(result_file, "%10000[^\n]", line);
        fscanf(result_file, "\n");
        line = strchr(line, (int) '{');
        if (!line)
          { fprintf(stderr, "\nError: Invalid file format of file "
                            "\'%s\'.\n\n", result_filename);
            fclose(result_file);
            fclose(output_file);
            remove(output_filename);
            return 1;
          }
        line++;
        // Read the tabulated nonterminals
        while ((q = (int) strtol(line, &line, 10)))
          { if (q > number.total)
              { fprintf(stderr, "\nError: Invalid file format of file "
                                "\'%s\'.\n\n", result_filename);
                fclose(result_file);
                fclose(output_file);
                remove(output_filename);
                return 1;
              }
            if (line[0] == 'l')
              { nonterms[q-1].space = 2;  // <-> O(n)
                line++;
              }
            else if (line[0] == 'c')
              { nonterms[q-1].space = 0;  // <-> 1
                line++;
              }
            else
              nonterms[q-1].space = 3;  // <-> O(n^2)
            
            // Tabulate the nonterminal
            GrayCount[q-1] = 0;
            v = (struct nt_vertex*) graph_vertex(graph,q);
            v->tabulated = 1;
            free(v->space);
            x = nonterms[q-1].space-1;
            if (x <= 0)
              x = 1;
            v->space = poly_n(x,POLY_SIZE);
          }
        // Maintain the list of table configurations. The following lines
        // also set the correct value of i (there is no "i++" missing ...) 
        if (!Tables)
          NEW_BEST_TABLECONF(Tables,CurTable,GrayCount,number.opt,i,
                             OptCard,Card)
        else
          APPEND_TABLECONF(CurTable,GrayCount,number.opt,i)
      }
    fclose(result_file);
      
    // Recompute the polynomial runtimes and the polynomial optimal table
    // configurations. Write this data do the output-file.
    Run_Type run_state = malloc(number.total*sizeof(struct run_type));
    for (i = 0; i < number.total; i++) 
      { run_state[i].runstate = -1;
        run_state[i].list = NULL;
      }
    char* tab_runtime = calloc(number.total,1);
    int* opt_nonterms = malloc(number.opt*sizeof(int));
    for (i = 0; i < number.opt; i++)
      opt_nonterms[i] = i;
    update_table_configuration(nonterms,number.total,GrayCount,opt_nonterms);
    int best = calc_asm_runtime(nonterms,0,number.total,EXPONENTIAL,
                                run_state,tab_runtime);
    print_asmopt_tableconf(output_file,nonterms,number,Tables,graph,
                           opt_nonterms,best);
    if (best < EXPONENTIAL)
      { Poly best_poly_runtime;
        unsigned int best_poly_count;                       
        get_best_polynomial_runtime(nonterms,number,Tables,graph,opt_nonterms,
                                    &best_poly_runtime,&best_poly_count);
        fprintf(output_file, "\nOptimal table configurations with respect ");
        fprintf(output_file, "to constant factors : %u\n", best_poly_count);
        print_polyopt_tableconf(output_file,nonterms,number,Tables,graph,
                                opt_nonterms,best_poly_runtime);
      }
    
    fclose(output_file);
    
    fprintf(stderr, "\nThe results from \'%s\' have been successfully updated "
                    "to \'%s\'.\n\n", result_filename, output_filename);
    
    return 0;
  }
