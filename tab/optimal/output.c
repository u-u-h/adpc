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

/*! \file output.c

    \brief Functions to write output-information to streams. They are used for
    \brief both screen-output and textfile-output.

*/


#include <output.h>


void print_nt_list (FILE* file, Asm_Nts nonterms, Nt_List nts)

  { fprintf(file, "  { ");
    while (nts != NULL)
      { fprintf(file, "%d",nts->nt+1);
        switch (nonterms[nts->nt].space)
          { case 0:
            case 1:
              fprintf(file, "c ");  // nt requires constant space
              break;
            case 2:
              fprintf(file, "l ");  // nt requires linear space
              break;
            case 3:
              fprintf(file, " ");   // nt requires square space
              break;
            default:
              fprintf(file, "ERROR");  // invalid space setting
              break;
          }
        nts = nts->next;
      }
    fprintf(file, "}\n");
  }


void print_table_configuration (FILE* file, Asm_Nts nonterms, int n)

  { int i;
    fprintf(file, "{ ");
    for (i = 0; i < n; i++)
      if (nonterms[i].tab == 0)
        { fprintf(file, "%d",i+1);
          switch (nonterms[i].space)
            { case 0:
              case 1:
                fprintf(file, "c ");  // nt requires constant space
                break;
              case 2:
                fprintf(file, "l ");  // nt requires linear space
                break;
              case 3:
                fprintf(file, " ");   // nt requires square space
                break;
              default:
                fprintf(file, "ERROR");  // invalid space setting
                break;
            }
        }
    fprintf(file, "}");
  }


void print_asm_dep_graph(FILE* file, Asm_Nts nonterms, int n)

  { int i;
    Asm_Deps deps;
    fprintf(file, "\n----------------------------------------"
                    "---------------------------------------\n");
    for(i = 0; i < n; i++)
      { fprintf(file, "nonterminal[%d]:\n",i+1);
        fprintf(file, "  nt      = %d\n", nonterms[i].nt+1);
        fprintf(file, "  tab     = ");
        switch (nonterms[i].tab)
          { case 0:
              fprintf(file, "tabulated\n");
              break;
            case 1:
              fprintf(file, "nontabulated\n");
              break;
            case 2:
              fprintf(file, "optimize\n");
              break;
            default:
              fprintf(file, "invalid value (%d)\n",nonterms[i].tab);
              break;
          }
        fprintf(file, "  runtime = %s\n", asm2string(nonterms[i].runtime));
        fprintf(file, "  space   = %s\n", asm2string(nonterms[i].space));
        fprintf(file, "  usage   = ");
        if (nonterms[i].usage > -1)
          fprintf(file, "%s\n", asm2string(nonterms[i].usage));
        else
          fprintf(file, "not in use\n");
        fprintf(file, "  dependencies:\n");
        deps = nonterms[i].deps;
        while (deps)
          { fprintf(file, "    - %s calls to nonterminal %d\n",
                            asm2string(deps->usages), deps->nt+1);
             deps = deps->next;
          }
        fprintf(file, "----------------------------------------"
                        "---------------------------------------\n");
      }
  }


void print_Code (FILE* file, char* code, int n)

  { int i;
    // Reverse order is in accord with the significance of the bits
    for (i = n-1; i >= 0; i--)
      if (code[i] == 0)
        fprintf(file, "0");
      else
        fprintf(file, "1");
  }
  

void comment_user_annotation(FILE* file, Runtime_Info runtime)

  { if (runtime.best < runtime.best_user)
      { fprintf(file, "\nNote: The user annotation increases the best");
        fprintf(file, "\n      possible runtime from %s to %s.\n",
                        asm2string(runtime.best),
                        asm2string(runtime.best_user));
      }
  }  


void write_resfile (FILE* file, Asm_Nts nonterms, Number_Info number,
                    Runtime_Info runtime, Nonterms_Info list,
                    TableDesign_List Tables, struct graph* graph,
                    int* opt_nonterms, int best, Opt_Rec* options,
                    int OptCard, unsigned int OptCount)

  { fprintf(file, "Total number of nonterminals            : %d\n",
                  number.total);
    fprintf(file, "Nonterminals which are not in use       : %d\n",
                  number.unused);
    if (number.unused > 0)
      print_nt_list(file, nonterms, list.unused);
    fprintf(file, "Nonterminals requiring < square space   : %d\n",
                  number.lin);
    if (number.lin > 0)
      print_nt_list(file, nonterms, list.lin);
    fprintf(file, "Nonterminals defined by user annotation : %d\n",
                  number.user);
    if (number.user > 0)
      print_nt_list(file, nonterms, list.user);
    fprintf(file, "Nonterminals defined by reductions      : %d\n",
                  number.red);
    if (number.red > 0)
      print_nt_list(file, nonterms, list.red);
    fprintf(file, "Nonterminals remaining for optimization : %d\n",
                  number.opt);
    if (number.opt > 0)
      print_nt_list(file, nonterms, list.opt);
    
    fprintf(file, "\nSome information about the runtime:\n");
    fprintf(file, "  tabulate all nonterminals      : %s\n",
                  asm2string(runtime.all));
    fprintf(file, "  tabulate no  nonterminals      : %s\n",
                  asm2string(runtime.no));
    fprintf(file, "  optimal runtime                : %s\n",
                  asm2string(runtime.best));
    fprintf(file, "Runtime with respect to the user annotation:\n");
    fprintf(file, "  tabulate all free nonterminals : %s\n",
                  asm2string(runtime.all_user));
    fprintf(file, "  tabulate no  free nonterminals : %s\n",
                  asm2string(runtime.no_user));
    fprintf(file, "  optimal runtime                : %s\n",
                  asm2string(runtime.best_user));
    comment_user_annotation(file, runtime);
    if (options && (options->bound > -1))
      fprintf(file, "\nOnly solutions with <= %d Tables "
                    "are considered.\n", options->bound);
    if (options && (options->exact > -1))
      fprintf(file, "\nOnly solutions with exact %d Tables "
                    "are considered.\n", options->exact);
    if (options && options->lin)
      fprintf(file, "\nEven linear and constant tables contribute to the "
                    "cardinality of the table\nconfiguration (not "
                    "only square tables).\n");
    if (options && options->nobest)
      fprintf(file, "\nDo not use the optimal runtime computed in advance "
                    "to prune the search space.\nCompute the runtime for "
                    "every table configuration in the search space.\n");
    if (options && (options->globalbest > -1))
      fprintf(file, "\nCalculate the best possible polynomial runtime "
                    "for all table configurations\nwith >= %d tables.\n",
                    options->globalbest);
    fprintf(file, "\nMinimum number of tables     : %d\n", OptCard);
    fprintf(file, "Optimal table configurations : %u\n", OptCount);
    print_asmopt_tableconf(file,nonterms,number,Tables,graph,opt_nonterms,
                           best);
    if (best < EXPONENTIAL)
      { Poly best_poly_runtime;
        unsigned int best_poly_count;                       
        get_best_polynomial_runtime(nonterms,number,Tables,graph,
                                    opt_nonterms,&best_poly_runtime,
                                    &best_poly_count);
        fprintf(file, "\nOptimal table configurations with respect ");
        fprintf(file, "to constant factors : %u\n", best_poly_count);
        print_polyopt_tableconf(file,nonterms,number,Tables,graph,
                                opt_nonterms,best_poly_runtime);
      }
  }


void print_asmopt_tableconf (FILE* file, Asm_Nts nonterms, Number_Info number,
                             TableDesign_List Tables, struct graph* graph,
                             int* opt_nonterms, int best)

  { Poly runtime_polynomial;
    TableDesign_List CurTable = Tables;
    int solution = 0;
    while (CurTable)
      { fprintf(file, "  ");
        update_table_configuration(nonterms,number.opt,CurTable->td,
                                   opt_nonterms);
        print_table_configuration(file,nonterms,number.total);
        if (best < EXPONENTIAL)
          { fprintf(file, "  (");
            convert_table_configuration(nonterms,number.total,graph);
            runtime_polynomial = compute_runtime(graph);
            poly_print(file, runtime_polynomial);
            fprintf(file, ")");
            free(runtime_polynomial);
          }
        fprintf(file, "\n");
        CurTable = CurTable->next;
        solution = 1;
      }
    if (!solution)
      fprintf(file, "  There is no solution.\n");
  }


void print_polyopt_tableconf (FILE* file, Asm_Nts nonterms, Number_Info number,
                              TableDesign_List Tables, struct graph* graph,
                              int* opt_nonterms, Poly best_runtime)

  { Poly runtime;
    TableDesign_List CurTable = Tables;
    int solution = 0;
    while (CurTable)
      { update_table_configuration(nonterms,number.opt,CurTable->td,
                                   opt_nonterms);
        convert_table_configuration(nonterms,number.total,graph);
        runtime = compute_runtime(graph);
        // Does best_runtime equal runtime ?
        if (poly_compare(best_runtime, runtime) == 0)
          { fprintf(file, "  ");
            print_table_configuration(file,nonterms,number.total);
            fprintf(file, "  (");
            poly_print(file, best_runtime);
            fprintf(file, ")\n");
          }
        free(runtime);
        CurTable = CurTable->next;
        solution = 1;
      }
    if (!solution)
      fprintf(file, "  There is no solution.\n");
  }


void print_details (FILE* file, Asm_Nts nonterms, int n, struct graph* graph,
                    Run_Type run_state, char* tab_runtime)
          
  { // Back up the usage-fields of the nonterminals
    Asm_Nts test_nonterms = malloc(n*sizeof(struct asm_nts));
    memcpy(test_nonterms, nonterms, n*sizeof(struct asm_nts));
    
    struct bag rec_bag;
    struct run_state **run_states;
    bag_init(&rec_bag, graph->vertex_count + 1);
    run_states = (struct run_state**) index_list_init(graph->vertex_count+1);
    Poly result;
    int result_asm, i;
    fprintf(file, "\n\nThe runtimes and usages of the nonterminals in detail:"
                  "\n");
    fprintf(file, "\n----------------------------------------"
                  "---------------------------------------\n");
    asm_get_usages(test_nonterms,n,0);
    for(i = 0; i < n; i++)
      { fprintf(file, "Nonterminal[%d", i+1);
        switch (nonterms[i].space)
          { case 0:
            case 1:
              fprintf(file, "c]:");
              break;
            case 2:
              fprintf(file, "l]:");
              break;
            case 3:
              fprintf(file, "]:");
               break;
            default:
              fprintf(file, "ERROR]:");
            break;
          }
        if (nonterms[i].usage == -1)
          fprintf(file, "  not in use\n");
        else
          { if (nonterms[i].tab == 0)
              fprintf(file, "  (tabulated)");
            result = runtime(i+1,graph,&rec_bag,run_states);
            result_asm = calc_asm_runtime_notab(nonterms,i,n,EXPONENTIAL,
                                                run_state,tab_runtime);
            fprintf(file, "\n  runtime : ");
            poly_print(file, result);
            fprintf(file, "  =  %s\n", asm2string(result_asm));
            fprintf(file, "  usage   : %s\n",
                          asm2string(test_nonterms[i].usage));
          }
        fprintf(file, "----------------------------------------"
                      "---------------------------------------\n");
      }
    free(test_nonterms);
  }


void print_time (FILE* file, double secTime)

  { double days = floor(secTime / 86400);   // 1 day == 86400 seconds
    int seconds = (int) ((double) (secTime) - (double) (days*86400));
    int hours = seconds / 3600;
    seconds = seconds % 3600;
    int minutes = seconds / 60;
    seconds = seconds % 60;
    if (days > 0)
      { if (hours == 23)
          { days++;
            hours = 0;
          }
        else
          hours++;
        fprintf(file, "%.0lf days %d hours", days, hours);
      }    
    else if (hours > 0)
     { if (minutes == 59)
         { hours++;
           minutes = 0;
         }
       else
         minutes++;
       fprintf(file, "%d hours %d minutes", hours, minutes);
     }
    else 
     { if (minutes > 0)
         fprintf(file, "%d minutes ", minutes);
       fprintf(file, "%d seconds", seconds);
     }
  }
