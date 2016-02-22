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

/*! \file taboptimal_in.c

    \brief The main program of the built-in-variant of the software for the
    \brief brute-force-calculation of the optimal table configuration.

*/

#include <taboptimal_in.h>


struct dep_graph * table_design_optimal (Nts inputdata, int log_level)

  { // Print a "the program is now runnig" - message ;-)
    if (log_level >= log_default)
      { fprintf(stdout, "\n\nCalculate the optimal table configuration ");
        fprintf(stdout, "of an ADP-program\n");
      }
    
    // Convert the inputdata to both the asymptotical representation and the
    // polynomial (graph) representation
    Asm_Nts nonterms;
    Number_Info number;
    Nonterms_Info list;
    convert_input(inputdata, &nonterms, &(number.total));
    Poly runtime_poly, best_poly;
    unsigned int best_poly_count;
    struct dep_graph* nonterms_graph;
    nonterms_graph = (struct dep_graph*) graph_init(50, 50, 10,
                                                    sizeof(struct dep_graph),
                                                    sizeof(struct nt_vertex),
                                                    sizeof(struct dep_edge),
                                                    NULL);
    struct graph* graph = &(nonterms_graph->graph);
    nts_to_graph(inputdata, graph);    
    
    // Get the usage of each nonterminal (with respect to the empty table
    // configuration)
    asm_get_usages(nonterms,number.total,0);  // nonterminal 0 <-> axiom
      
    // Declare and initialize two global arrays that are used in
    // "calc_asm_runtime" (Declaring them global avoids allocating and freeing
    // memory each time "calc_asm_runtime" is called => faster)
    Run_Type run_state = malloc(number.total*sizeof(struct run_type));
    int i;
    for (i = 0; i < number.total; i++) 
      { run_state[i].runstate = -1;
        run_state[i].list = NULL;
      }
    char* tab_runtime = calloc(number.total,1);
    
    
    // Calculate and print some general runtime-information
    Runtime_Info runtime;
    Asm_Nts test_nonterms = malloc(number.total*sizeof(struct asm_nts));
    memcpy(test_nonterms, nonterms, number.total*sizeof(struct asm_nts));
    if (log_level >= log_default)
      fprintf(stdout, "\nSome information about the runtime in advance:\n");
    for (i = 0; i < number.total; i++) 
      test_nonterms[i].tab = 0;  // tabulate every nonterminal
    runtime.all = calc_asm_runtime(test_nonterms,0,number.total,EXPONENTIAL,
                                   run_state,tab_runtime);
    if (log_level >= log_default)
      fprintf(stdout, "  tabulate all nonterminals      : %s\n",
                      asm2string(runtime.all));
    for (i = 0; i < number.total; i++) 
      test_nonterms[i].tab = 1;  // tabulate no nonterminal
    runtime.no = calc_asm_runtime(test_nonterms,0,number.total,EXPONENTIAL,
                                  run_state,tab_runtime);
    if (log_level >= log_default)
      fprintf(stdout, "  tabulate no  nonterminals      : %s\n",
                      asm2string(runtime.no));
    for (i = 0; i < number.total; i++) 
      test_nonterms[i].tab = 2;  // no user annotation
    runtime.best = asm_best_runtime(test_nonterms,number.total,run_state,
                                    tab_runtime);
    if (log_level >= log_default)
      fprintf(stdout, "  optimal runtime                : %s\n",
                      asm2string(runtime.best));
    for (i = 0; i < number.total; i++)  // tabulate all free nonterminals
      test_nonterms[i].tab = (nonterms[i].tab == 2) ? 0 : nonterms[i].tab;
    runtime.all_user = calc_asm_runtime(test_nonterms,0,number.total,
                                        EXPONENTIAL,run_state,tab_runtime);
    if (log_level >= log_default)
      { fprintf(stdout, "Runtime with respect to the user annotation:\n");
        fprintf(stdout, "  tabulate all free nonterminals : %s\n",
                        asm2string(runtime.all_user));
      }
    for (i = 0; i < number.total; i++)  // tabulate no free nonterminals
      test_nonterms[i].tab = (nonterms[i].tab == 2) ? 1 : nonterms[i].tab;
    runtime.no_user = calc_asm_runtime(test_nonterms,0,number.total,
                                       EXPONENTIAL,run_state,tab_runtime);
    if (log_level >= log_default)
      fprintf(stdout, "  tabulate no  free nonterminals : %s\n",
                      asm2string(runtime.no_user));
    // Determine the best possible runtime with respect to the user annotation
    runtime.best_user = asm_best_runtime(nonterms,number.total,run_state,
                                         tab_runtime);
    if (log_level >= log_default)
      fprintf(stdout, "  optimal runtime                : %s\n",
                      asm2string(runtime.best_user));
    free(test_nonterms);

    
    // Determine which nonterminal belongs to which categorie:
    // Use an array to represent the optimization-state of each nonterminal:
    //   opt_nontermstate[q] == 0 <-> q remains for optimization
    //   opt_nontermstate[q] == 1 <-> q's tabulation state is defined
    //                                otherwise and it does not remain for
    //                                optimization
    int* opt_nontermstate = calloc(number.total,sizeof(int));
    
    // Consider user annotation
    set_user_configuration(nonterms,number.total,opt_nontermstate,
                           &(number.user),&(list.user));
    // Determine unused nonterminals
    list_unused_nonterms(nonterms,number.total,opt_nontermstate,
                         &(number.unused),&(list.unused));
    // Determine nonterminals that require less than square space
    list_lin_nonterms(nonterms,number.total,&(number.lin),&(list.lin));
    number.used = number.total - number.unused;
    // Apply preprocessing reductions
    asm_apply_reductions(nonterms,number.total,run_state,tab_runtime,
                         opt_nontermstate,&(number.red),&(list.red),NULL);
    
    // Use an "array-embedded list" that contains all optimization-
    // nonterminals. opt_nonterms[i] == q <-> q is the i-th optimization-
    // nonterminal. The number of optimization-nonterminals is number.opt
    int* opt_nonterms = malloc(number.total*sizeof(int));
    number.opt = 0;
    set_opt_nonterms(opt_nontermstate,number.total,opt_nonterms,&(number.opt),
                     &(list.opt));
                           
    // Print some information about the categories of the nonterminals
    if (log_level >= log_default)
      { fprintf(stdout, "\nTotal number of nonterminals            : %d\n",
                        number.total);
        fprintf(stdout, "Nonterminals which are not in use       : %d\n",
                        number.unused);
        if (number.unused > 0)
          print_nt_list(stdout,nonterms,list.unused);
        fprintf(stdout, "Nonterminals requiring < square space   : %d\n",
                        number.lin);
        if (number.lin > 0)
          print_nt_list(stdout,nonterms,list.lin);
        fprintf(stdout, "Nonterminals defined by user annotation : %d\n",
                        number.user);
        if (number.user > 0)
          print_nt_list(stdout,nonterms,list.user);
        fprintf(stdout, "Nonterminals defined by reductions      : %d\n",
                        number.red);
        if (number.red > 0)
          print_nt_list(stdout,nonterms,list.red);
        fprintf(stdout, "Nonterminals remaining for optimization : %d\n",
                        number.opt);
        if (number.opt > 0)
          print_nt_list(stdout,nonterms,list.opt);
        // Print a hint if the user annotation increases the best possible
        // runtime asymptotically
        comment_user_annotation(stdout,runtime);
      }
    
    
    // CardCount[q] = 1 <-> The optimization-nonterminal q contributes to the
    //                      cardinality of the table configuration (if it is
    //                      tabulated)
    // CardCount[q] = 0 <-> It does not contribute to the cardinality
    int* CardCount = malloc(number.opt*sizeof(int));
    for (i = 0; i < number.opt; i++)
      if (nonterms[opt_nonterms[i]].space == 3)
        CardCount[i] = 1;
      else
        CardCount[i] = 0;
    
    // Set the initial table configuration of the optimization-nonterminals:
    // tabulate each of them
    for (i = 0; i < number.opt; i++)
      nonterms[opt_nonterms[i]].tab = 0;
    int OptCard = 0;  // The cardinality of the optimal table configuration(s)
    for (i = 0; i < number.total; i++)
      if ((nonterms[i].usage > -1) && (nonterms[i].tab == 0) &&
          (nonterms[i].space == 3))
         OptCard++;
        
    // Tables is the list of all asymptotically optimal table configurations
    TableDesign_List Tables;
    // Set the result-file
    
    int best = runtime.best_user;  // best possible asymptotical runtime
    
    
    // Is the table configuration already completely set ? (i.e. there is no
    // nonterminal remaining for optimization)
    if (number.opt == 0)
      { convert_table_configuration(nonterms,number.total,graph);
        // Print the result
        if (log_level >= log_default)
          { fprintf(stdout, "\nAll nonterminals are allready defined.\n");
            fprintf(stdout, "Minimum number of tables     : %d\n",
                            OptCard);
            fprintf(stdout, "Optimal table configurations : 1\n");
            fprintf(stdout, "  ");
            print_table_configuration(stdout,nonterms,number.total);
            if (best < EXPONENTIAL)
              { fprintf(stdout, "  (");
                runtime_poly = compute_runtime(graph);
                poly_print(stdout,runtime_poly);
                fprintf(stdout, ")");
              }
            fprintf(stdout, "\n");
          }
        // Print the runtime and usage of each nonterminal in detail
        if (log_level >= log_verbose)
          print_details(stdout,nonterms,number.total,graph,run_state,
                        tab_runtime);
        // Write the result-file
        // Tables and opt_nonterms are dummies here.
        Tables = malloc(sizeof(struct tabledesign_list));
        Tables->td = NULL;
        Tables->next = NULL;
        opt_nonterms = NULL;
        
        return nonterms_graph;
      }

    
    // Initializations for the brute-force-optimization. We enumerate all
    // 2^number.opt table configurations in the order of the Gray-Code. This
    // way we have to change the tabulation state of exact one nonterminal
    // only from one iteration to the next. We maintain a Binary-Code-
    // representation as well to determine the position in the Gray-Code where
    // the bit changes (this corresponds to the optimization-nonterminal whose
    // tabulation state changes).
    
    // Initialize the Code-counters
    char* BinaryCount = calloc(number.opt,1);
    char* GrayCount = calloc(number.opt,1);
    // Do not tabulate the first optimization-nontermial in the
    // initialization. This is done for technical reasons only. In the first
    // iteration of the brute-force-optimization this is changed and the full
    // table configuration of all optimization-nontermianls is the first that
    // is considerd.
    GrayCount[0] = 1;
    nonterms[opt_nonterms[0]].tab = 1;
    // chPos indicates the position of the last change in the BinaryCount.
    // Card is the cardinality of the table configuration considered
    // currently.
    int chPos = 0, Card = OptCard - CardCount[0];
    unsigned int OptCount = 0,  // Number of optimal table configurations
                 UpdateCount = 0;  // Number of solution updates
    // Initialize a dummy entry in the list table configurations
    Tables = malloc(sizeof(struct tabledesign_list));
    TableDesign_List CurTable = Tables;
    Tables->td = NULL;
    Tables->next = NULL;
    // Initialize the variables to compute the elapsed runtime and the
    // estimated remaining runtime of the brute-force-optimization
    // Counter1 ... Number of iterations since the last time-check.
    // Counter2 ... Number of time-checks since the last runtime-estimation
    unsigned int Counter1 = 0, Counter2 = 1;
    // If "Counter1 > NumTime" then a new time-check is performed
    unsigned int NumTime = INIT_NUMTIME;
    double NumIterations = pow(2,number.opt);  // Total number of iterations
    double CountIterations = 0;  // Number of iteration from the start of the
                                 // brute-force-optimization to the last time-
                                 // check
    double remTime;  // estimated remaining runtime
    time_t StartTime = time(NULL),  // Starttime of the brute-force-
                                    // optimization
           StartEstiTime = StartTime,  // Time of the last runtime-estimation
           EndEstiTime;  // Time of the last time-check
    
    
    // The brute-force-optimization
    if (log_level >= log_default)
      { fprintf(stdout, "\nRunning brute-force-optimization. ");
        fprintf(stdout, "Please wait ...\n");
      }
    convert_table_configuration(nonterms, number.total, graph);
    
    // Find the asymptotically optimal table configurations
    do
      { // Update table configuration
        GrayCount[chPos] = 1 - GrayCount[chPos];
        nonterms[opt_nonterms[chPos]].tab = GrayCount[chPos];
        if (GrayCount[chPos] == 0)
          Card += CardCount[chPos]; 
        else
          Card -= CardCount[chPos]; 
        // Calculate runtime and update solutions
        if (Card <= OptCard)
          { if (calc_asm_runtime(nonterms,0,number.total,best,run_state,
                                 tab_runtime) <= best)
              { if (Card == OptCard)
                  APPEND_TABLECONF(CurTable,GrayCount,number.opt,OptCount)
                else
                  NEW_BEST_TABLECONF(Tables,CurTable,GrayCount,number.opt,
                                     OptCount,OptCard,Card)
                if (log_level >= log_verbose)
                  PRINT_UPDATE_INFO(stdout,UpdateCount,CountIterations,
                                    Counter1,OptCard,nonterms,number.total,
                                    NULL)
              }
          }
        // Calculate next table configuration
        CODE_COUNT(BinaryCount,number.opt,chPos)
        // Runtime-estimation
        Counter1++;
        if ((Counter1 == NumTime) && (log_level >= log_default))
          { EndEstiTime = time(NULL);
            CountIterations += Counter1;
            Counter1 = 0;
            if (difftime(EndEstiTime, StartEstiTime) >= ESTIMATE_TIME)
              { remTime = (NumIterations/CountIterations) *
                  difftime(EndEstiTime, StartTime)
                    - difftime(EndEstiTime, StartTime);
                fprintf(stdout, "\nCurrent iteration           : ");
                fprintf(stdout, "%.0lf/%.0lf => %.2lf %%\n",
                       (double) CountIterations, (double) NumIterations,
                       (double) (CountIterations/NumIterations*100));
                fprintf(stdout, "Runtime so far              : ");
                print_time(stdout,
                          (long double) (difftime(EndEstiTime, StartTime)));
                fprintf(stdout, "\n");
                fprintf(stdout, "Estimated remaining runtime : ");
                print_time(stdout, remTime);
                fprintf(stdout, "\n");
                fprintf(stdout, "One intermediate solution   : %d Tables\n",
                                OptCard);
                update_table_configuration(nonterms,number.opt,CurTable->td,
                                           opt_nonterms);
                fprintf(stdout, "  ");
                print_table_configuration(stdout,nonterms,number.total);
                fprintf(stdout, "\n");
                update_table_configuration(nonterms,number.opt,GrayCount,
                                           opt_nonterms);
                StartEstiTime = EndEstiTime;
                if (Counter2 > 4)
                  NumTime *= 2;
                else if (Counter2 < 2)
                  NumTime /= 2;
                Counter2 = 0;
              }
            else Counter2++;
          }
      } 
    while (chPos != number.opt);
    
    // Remove the dummy entry in the list of the optimal table configurations
    if (!(Tables->td))
      { CurTable = Tables;
        Tables = Tables->next;
        free(CurTable);
      }       
    
    // Print the result
    if (log_level >= log_default) {
        // Print the actual used runtime
        fprintf(stdout, "\nReady! Actual runtime        : ");
        time_t EndTime = time(NULL);
        print_time(stdout, difftime(EndTime, StartTime));
        fprintf(stdout, "\n");
        // Print the result
        fprintf(stdout, "\nOptimal runtime              : %s\n",
                        asm2string(best));
        fprintf(stdout, "Minimum number of tables     : %d\n", OptCard);
        fprintf(stdout, "Optimal table configurations : %u\n", OptCount);
        print_asmopt_tableconf(stdout,nonterms,number,Tables,graph,
                               opt_nonterms,best);
      }

    // Need this because we return nonterms_graph - independent of the
    // log_level
    get_best_polynomial_runtime(nonterms,number,Tables,graph,opt_nonterms,
                                    &best_poly,&best_poly_count);

    if (log_level >= log_default) {
        if (best < EXPONENTIAL)
          { fprintf(stdout, "\nOptimal table configurations with respect ");
            fprintf(stdout, "to constant factors : %u\n", best_poly_count);
            print_polyopt_tableconf(stdout,nonterms,number,Tables,graph,
                                    opt_nonterms,best_poly);
          }
      }
    
    return nonterms_graph;
  }
