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

/*! \file taboptimal.c

    \brief The main program of the stand-alone-variant of the software for the
    \brief brute-force-calculation of the optimal table configuration.

*/


#include <taboptimal.h>


int main (int argc, char** argv)

  { // Get and handle the commandline-options
    char* progname = argv[0];
    Opt_Rec options;
    handle_command_line_options(argc,argv,progname,TABRESULT_FILENAME,
                                &options);

#ifndef WINDOWS
    // Set up the signal handler and initialize the correlated variables
    extern volatile int ext_signal;
    ext_signal = 0;
    struct sigaction new_action;
    setup_signal_handler(options);
#endif
#ifdef WINDOWS
    int key;  // The ASCII-Code of the last key the user pressed. It is used
              // as an "local" variable of the macro HANDLE_SIGNAL_TIME.
#endif

    // Print a "the program is now runnig" - message ;-)
    int i, log_level = options.verbose;
    if (log_level >= log_default)
      { fprintf(stderr, "\n\nCalculate the optimal table configuration ");
        fprintf(stderr, "of an ADP-program\n");
      }

    // Open the input-stream
    FILE* inputfile;
    if (strcmp(options.inputfilename,"-") == 0)
      inputfile = stdin;
    else
      inputfile = fopen(options.inputfilename,"r");
    if (!inputfile)
      { fprintf(stderr, "\nError: Cannot open file \'%s\'.\n\n",
                         options.inputfilename);
        return 1;
      }

    // Read the nonterminals and the parser-dependencies from the input-stream
    Asm_Nts nonterms;
    Number_Info number;
    Nonterms_Info list;
    int* serialized_arr;
    int arr_size;
    read_array(inputfile,&serialized_arr,&arr_size);
    if (!serialized_arr)
      { fprintf(stderr, "\nError: Cannot read data from \'%s\': "
                        "invalid file-format.\n\n",
                        options.inputfilename);
        return 1;
      }

    // Deserialize the input-data and convert it to both the asymptotical
    // representation and the polynomial (graph) representation
    Nts inputdata = deserializeGraph(serialized_arr,&arr_size);
    convert_input(inputdata, &nonterms, &(number.total));
    Poly new_poly, best_poly;
    unsigned int best_poly_count;
    struct dep_graph* nonterms_graph;
    nonterms_graph = (struct dep_graph*) graph_init(50, 50, 10,
                                                    sizeof(struct dep_graph),
                                                    sizeof(struct nt_vertex),
                                                    sizeof(struct dep_edge),
                                                    NULL);
    struct graph* graph = &(nonterms_graph->graph);
    nts_to_graph(inputdata, graph);
    struct nt_vertex* v;

    // Consider some of these commandline-options that influences the
    // nonterminals
    if (!options.user)
      for (i = 0; i < number.total; i++)
        nonterms[i].tab = 2;
    if (options.square)
      for (i = 0; i < number.total; i++)
        nonterms[i].space = 3;  // <-> O(n^2)
    if (options.TableDef == 1)  // given table configuration
      { for (i = 0; i < number.total; i++)
          nonterms[i].tab = 1;
        while (options.TableConf)
          { Nt_List auxlist = options.TableConf;
            if ((auxlist->nt > 0) && (auxlist->nt <= number.total))
              nonterms[auxlist->nt-1].tab = 0;
            options.TableConf = options.TableConf->next;
            free(auxlist);
          }
      }
    if (options.TableDef == 2)  // command-line annotations
      { while (options.TableConf)
          { Nt_List auxlist = options.TableConf;
            if ((auxlist->nt > 0) && (auxlist->nt <= number.total))
              nonterms[auxlist->nt-1].tab = 0;
            else if ((-(auxlist->nt) > 0) && (-(auxlist->nt) <= number.total))
              nonterms[-(auxlist->nt)-1].tab = 1;
            options.TableConf = options.TableConf->next;
            free(auxlist);
          }
      }

    // Get the usage of each nonterminal (with respect to the empty table
    // configuration)
    asm_get_usages(nonterms,number.total,0);  // nonterminal 0 <-> axiom

    // Print debug-information if desired
    if (options.print_dep_graph)
      { print_asm_dep_graph(stderr, nonterms, number.total);
        return 0;
      }

    // Declare and initialize two global arrays that are used in
    // "calc_asm_runtime" (Declaring them global avoids allocating and freeing
    // memory each time "calc_asm_runtime" is called => faster)
    Run_Type run_state = malloc(number.total*sizeof(struct run_type));
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
      fprintf(stderr, "\nSome information about the runtime in advance:\n");
    for (i = 0; i < number.total; i++)
      test_nonterms[i].tab = 0;  // tabulate every nonterminal
    runtime.all = calc_asm_runtime(test_nonterms,0,number.total,EXPONENTIAL,
                                   run_state,tab_runtime);
    if (log_level >= log_default)
      fprintf(stderr, "  tabulate all nonterminals      : %s\n",
                      asm2string(runtime.all));
    for (i = 0; i < number.total; i++)
      test_nonterms[i].tab = 1;  // tabulate no nonterminal
    runtime.no = calc_asm_runtime(test_nonterms,0,number.total,EXPONENTIAL,
                                  run_state,tab_runtime);
    if (log_level >= log_default)
      fprintf(stderr, "  tabulate no  nonterminals      : %s\n",
                      asm2string(runtime.no));
    for (i = 0; i < number.total; i++)
      test_nonterms[i].tab = 2;  // no user annotation
    runtime.best = asm_best_runtime(test_nonterms,number.total,run_state,
                                    tab_runtime);
    if (log_level >= log_default)
      fprintf(stderr, "  optimal runtime                : %s\n",
                      asm2string(runtime.best));
    for (i = 0; i < number.total; i++)  // tabulate all free nonterminals
      test_nonterms[i].tab = (nonterms[i].tab == 2) ? 0 : nonterms[i].tab;
    runtime.all_user = calc_asm_runtime(test_nonterms,0,number.total,
                                        EXPONENTIAL,run_state,tab_runtime);
    if (log_level >= log_default)
      { fprintf(stderr, "Runtime with respect to the user annotation:\n");
        fprintf(stderr, "  tabulate all free nonterminals : %s\n",
                        asm2string(runtime.all_user));
      }
    for (i = 0; i < number.total; i++)  // tabulate no free nonterminals
      test_nonterms[i].tab = (nonterms[i].tab == 2) ? 1 : nonterms[i].tab;
    runtime.no_user = calc_asm_runtime(test_nonterms,0,number.total,
                                       EXPONENTIAL,run_state,tab_runtime);
    if (log_level >= log_default)
      fprintf(stderr, "  tabulate no  free nonterminals : %s\n",
                      asm2string(runtime.no_user));
    // Determine the best possible runtime with respect to the user annotation
    runtime.best_user = asm_best_runtime(nonterms,number.total,run_state,
                                         tab_runtime);
    if (log_level >= log_default)
      fprintf(stderr, "  optimal runtime                : %s\n",
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
    if (options.user || options.TableDef)
      set_user_configuration(nonterms,number.total,opt_nontermstate,
                             &(number.user),&(list.user));
    else
      { number.user = 0;
        list.user = NULL;
      }
    // Determine unused nonterminals
    list_unused_nonterms(nonterms,number.total,opt_nontermstate,
                         &(number.unused),&(list.unused));
    // Determine nonterminals that require less than square space
    list_lin_nonterms(nonterms,number.total,&(number.lin),&(list.lin));
    number.used = number.total - number.unused;
    // Apply preprocessing reductions
    if (options.red)
      asm_apply_reductions(nonterms,number.total,run_state,tab_runtime,
                           opt_nontermstate,&(number.red),&(list.red),
                           &options);
    else
      { number.red = 0;
        list.red = NULL;
      }
    // Use an "array-embedded list" that contains all optimization-
    // nonterminals. opt_nonterms[i] == q <-> q is the i-th optimization-
    // nonterminal. The number of optimization-nonterminals is number.opt
    int* opt_nonterms = malloc(number.total*sizeof(int));
    number.opt = 0;
    set_opt_nonterms(opt_nontermstate,number.total,opt_nonterms,&(number.opt),
                     &(list.opt));
                           
    // Print some information about the categories of the nonterminals
    if (log_level >= log_default)
      { fprintf(stderr, "\nTotal number of nonterminals            : %d\n",
                        number.total);
        fprintf(stderr, "Nonterminals which are not in use       : %d\n",
                        number.unused);
        if (number.unused > 0)
          print_nt_list(stderr,nonterms,list.unused);
        fprintf(stderr, "Nonterminals requiring < square space   : %d\n",
                        number.lin);
        if (number.lin > 0)
          print_nt_list(stderr,nonterms,list.lin);
        fprintf(stderr, "Nonterminals defined by user annotation : %d\n",
                        number.user);
        if (number.user > 0)
          print_nt_list(stderr,nonterms,list.user);
        fprintf(stderr, "Nonterminals defined by reductions      : %d\n",
                        number.red);
        if (number.red > 0)
          print_nt_list(stderr,nonterms,list.red);
        fprintf(stderr, "Nonterminals remaining for optimization : %d\n",
                        number.opt);
        if (number.opt > 0)
          print_nt_list(stderr,nonterms,list.opt);
        // Print a hint if the user annotation increases the best possible
        // runtime asymptotically
        comment_user_annotation(stderr,runtime);
      }
    
    
    // CardCount[q] = 1 <-> The optimization-nonterminal q contributes to the
    //                      cardinality of the table configuration (if it is
    //                      tabulated)
    // CardCount[q] = 0 <-> It does not contribute to the cardinality
    int* CardCount = malloc(number.opt*sizeof(int));
    for (i = 0; i < number.opt; i++)
      if (options.lin || (nonterms[opt_nonterms[i]].space == 3))
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
          (options.lin || (nonterms[i].space == 3)))
         OptCard++;
        
    // Tables is the list of all asymptotically optimal table configurations
    TableDesign_List Tables;
    // Set the result-file
    FILE* resfile;
    if (strcmp(options.resultfilename, "-") == 0)
      resfile = stdout;
    else
      resfile = fopen(options.resultfilename, "w");
    
    int best = runtime.best_user;  // best possible asymptotical runtime
    
    
    // Is the table configuration already completely set ? (i.e. there is no
    // nonterminal remaining for optimization)
    if (number.opt == 0)
      { convert_table_configuration(nonterms,number.total,graph);
        // Print the result
        if (log_level >= log_default)
          { fprintf(stderr, "\nAll nonterminals are allready defined.\n");
            fprintf(stderr, "Minimum number of tables     : %d\n",
                            OptCard);
            fprintf(stderr, "Optimal table configurations : 1\n");
            fprintf(stderr, "  ");
            print_table_configuration(stderr,nonterms,number.total);
            if (best < EXPONENTIAL)
              { fprintf(stderr, "  (");
                new_poly = compute_runtime(graph);
                poly_print(stderr,new_poly);
                fprintf(stderr, ")");
              }
            fprintf(stderr, "\n");
          }
        // Print the runtime and usage of each nonterminal in detail
        if (log_level >= log_verbose)
          print_details(stderr,nonterms,number.total,graph,run_state,
                        tab_runtime);
        // Write the result-file
        if (options.resultfilename)
          { // Tables and opt_nonterms are dummies here.
            Tables = malloc(sizeof(struct tabledesign_list));
            Tables->td = NULL;
            Tables->next = NULL;
            opt_nonterms = NULL;
            if (resfile)
              { write_resfile(resfile,nonterms,number,runtime,list,Tables,
                              graph,opt_nonterms,best,&options,OptCard,1);
                fclose(resfile);
              }
            else
              { fprintf(stderr, "\nError: Cannot create file \'%s\'.\n\n",
                                options.resultfilename);
                return 1;
              }
          }
        
        return 0;
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
    // The lower bound of the number of Tables is required for the option "-g"
    // only.
    int Card_lowerbound = options.globalbest;
    int new;  // Runtime of the table configuration considered currently
    best_poly = poly_exp();  // Initial value of the best polynomial runtime
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
    // If "Counter1 > NumTime" then a new time-check is performed. NumTime is
    // declared extern because it is involved in signal-handling (see
    // "asm_typedefs.h" and "signal_handler").
#ifndef WINDOWS
    extern volatile unsigned int NumTime;
#endif
#ifdef WINDOWS
    unsigned int NumTime;
#endif
    int estimate_time = options.time;  // Timeinterval between two runtime-
                                       // estimations
    NumTime = INIT_NUMTIME * estimate_time / ESTIMATE_TIME;
    double NumIterations = pow(2,number.opt);  // Total number of iterations
    double CountIterations = 0;  // Number of iteration from the start of the
                                 // brute-force-optimization to the last time-
                                 // check
    double remTime;  // estimated remaining runtime
    time_t StartTime = time(NULL),  // Starttime of the brute-force-
                                    // optimization
           StopTime = StartTime,  // Time when the brute-force-optimization is
                                  // stopped by stopping or terminating the
                                  // process (only if this happens)
           StartEstiTime = StartTime,  // Time of the last runtime-estimation
           EndEstiTime;  // Time of the last time-check
    FILE* statefile = NULL;  // program's execution state file
    
    // Consider commandline-options
    if (options.bound > -1) 
      { OptCard = options.bound;
        if (log_level >= log_default)
          fprintf(stderr, "\nOnly consider table configurations with <= %d "
                          "Tables.\n", OptCard);
      }
    if (options.exact > -1)
      { OptCard = options.exact;
        best = EXPONENTIAL;
        if (log_level >= log_default)
          fprintf(stderr, "\nOnly consider table configurations with exact %d "
                          "Tables.\n", OptCard);
      }
    if (options.lin && (log_level >= log_default))
      fprintf(stderr, "\nEven linear and constant tables contribute to the "
                      "cardinality of the table\nconfiguration (not "
                      "only square tables).\n");
    if (options.nobest && (log_level >= log_default))
      fprintf(stderr, "\nDo not use the optimal runtime computed in advance "
                      "to prune the search space.\nCompute the runtime for "
                      "every table configuration in the search space.\n");
    if ((options.globalbest > -1) && (log_level >= log_default))
      fprintf(stderr, "\nCalculate the best possible polynomial runtime "
                      "for all table configurations\nwith >= %d tables.\n",
                      Card_lowerbound);
    if (options.statefilename)
      { if ((statefile = fopen(options.statefilename,"r")))
          // If you do some changes: An appropriate amount of memory has to be
          // allocated for best_poly.
          get_execution_state(statefile,log_level,&options,&number,
                              opt_nontermstate,GrayCount,BinaryCount,&chPos,
                              &Card,&OptCard,&OptCount,&UpdateCount,&best,
                              &best_poly,&Tables,&CurTable,&Counter1,
                              &Counter2,&CountIterations,&StartTime,&StopTime,
                              &StartEstiTime,nonterms,opt_nonterms,runtime);
        // If "statefile == NULL" then the brute-force-optimization starts new
      }

    
    // The brute-force-optimization
    if (log_level >= log_default)
      { fprintf(stderr, "\nRunning brute-force-optimization. ");
        fprintf(stderr, "Please wait ...\n");
      }
    convert_table_configuration(nonterms, number.total, graph);

    // Standard: Find the asymptotically optimal table configurations
    if ((options.exact == -1) && (!options.nobest) && 
        (options.globalbest == -1))
      { do
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
                      PRINT_UPDATE_INFO(stderr,UpdateCount,CountIterations,
                                        Counter1,OptCard,nonterms,
                                        number.total,NULL)
                  }
              }
            // Calculate next table configuration
            CODE_COUNT(BinaryCount,number.opt,chPos)
            // Handle signals and the runtime-estimation
#ifndef WINDOWS
            HANDLE_SIGNAL_TIME(stderr,nonterms,number,log_level,options,
                               statefile,ext_signal,new_action,Counter1,
                               Counter2,CountIterations,NumTime,NumIterations,
                               remTime,StartTime,StopTime,StartEstiTime,
                               EndEstiTime,estimate_time,best,best_poly,
                               opt_nontermstate,opt_nonterms,GrayCount,
                               BinaryCount,chPos,Card,OptCard,OptCount,
                               UpdateCount,Tables,CurTable)
#endif
#ifdef WINDOWS
            HANDLE_SIGNAL_TIME(stderr,nonterms,number,log_level,options,
                               statefile,Counter1,Counter2,CountIterations,
                               NumTime,NumIterations,remTime,StartTime,
                               StopTime,StartEstiTime,EndEstiTime,
                               estimate_time,best,best_poly,opt_nontermstate,
                               opt_nonterms,GrayCount,BinaryCount,chPos,Card,
                               OptCard,OptCount,UpdateCount,Tables,CurTable,
                               key)
#endif
          }
        while (chPos != number.opt);
      }

    // Only consider table configurations with an exact number of tables
    // (option "-e")
    else if (options.exact > -1)
      { do
          { // Update table configuration
            GrayCount[chPos] = 1 - GrayCount[chPos];
            nonterms[opt_nonterms[chPos]].tab = GrayCount[chPos];
            if (GrayCount[chPos] == 0)
              Card += CardCount[chPos];
            else
              Card -= CardCount[chPos];
            // Calculate runtime and update solutions
            if (Card == OptCard)
              { new = calc_asm_runtime(nonterms,0,number.total,best,run_state,
                                       tab_runtime);
                if (new <= best)
                  { if (new == best)
                      APPEND_TABLECONF(CurTable,GrayCount,number.opt,OptCount)
                    else
                      { NEW_BEST_TABLECONF(Tables,CurTable,GrayCount,
                                           number.opt,OptCount,OptCard,Card)
                        best = new;
                      }
                    if (log_level >= log_verbose)
                      PRINT_UPDATE_INFO(stderr,UpdateCount,CountIterations,
                                        Counter1,OptCard,nonterms,
                                        number.total,NULL)
                  }
              }
            // Calculate next table configuration
            CODE_COUNT(BinaryCount,number.opt,chPos)
            // Handle signals and the runtime-estimation
#ifndef WINDOWS
            HANDLE_SIGNAL_TIME(stderr,nonterms,number,log_level,options,
                               statefile,ext_signal,new_action,Counter1,
                               Counter2,CountIterations,NumTime,NumIterations,
                               remTime,StartTime,StopTime,StartEstiTime,
                               EndEstiTime,estimate_time,best,best_poly,
                               opt_nontermstate,opt_nonterms,GrayCount,
                               BinaryCount,chPos,Card,OptCard,OptCount,
                               UpdateCount,Tables,CurTable)
#endif
#ifdef WINDOWS
            HANDLE_SIGNAL_TIME(stderr,nonterms,number,log_level,options,
                               statefile,Counter1,Counter2,CountIterations,
                               NumTime,NumIterations,remTime,StartTime,
                               StopTime,StartEstiTime,EndEstiTime,
                               estimate_time,best,best_poly,opt_nontermstate,
                               opt_nonterms,GrayCount,BinaryCount,chPos,Card,
                               OptCard,OptCount,UpdateCount,Tables,CurTable,
                               key)
#endif
          }
        while (chPos != number.opt);
      }

    // Consider every table configuration in the search space; do not prune
    // the search space with help of the best possible runtime computed in
    // advance (option "-n")
    else if (options.nobest)
      { do
          { // Update table configuration
            GrayCount[chPos] = 1 - GrayCount[chPos];
            nonterms[opt_nonterms[chPos]].tab = GrayCount[chPos];
            if (GrayCount[chPos] == 0)
              Card += CardCount[chPos];
            else
              Card -= CardCount[chPos];
            // Calculate runtime and update solutions
            new = calc_asm_runtime(nonterms,0,number.total,best,run_state,
                                   tab_runtime);
            if (new <= best)
              { if ((new < best) || (Card < OptCard))
                  { NEW_BEST_TABLECONF(Tables,CurTable,GrayCount,
                                       number.opt,OptCount,OptCard,Card)
                    best = new;
                  }
                else if (Card == OptCard)
                  APPEND_TABLECONF(CurTable,GrayCount,number.opt,
                                   OptCount)
                if (log_level >= log_verbose)
                  PRINT_UPDATE_INFO(stderr,UpdateCount,CountIterations,
                                    Counter1,OptCard,nonterms,number.total,
                                    NULL)
              }
            // Calculate next table configuration
            CODE_COUNT(BinaryCount,number.opt,chPos)
            // Handle signals and the runtime-estimation
#ifndef WINDOWS
            HANDLE_SIGNAL_TIME(stderr,nonterms,number,log_level,options,
                               statefile,ext_signal,new_action,Counter1,
                               Counter2,CountIterations,NumTime,NumIterations,
                               remTime,StartTime,StopTime,StartEstiTime,
                               EndEstiTime,estimate_time,best,best_poly,
                               opt_nontermstate,opt_nonterms,GrayCount,
                               BinaryCount,chPos,Card,OptCard,OptCount,
                               UpdateCount,Tables,CurTable)
#endif
#ifdef WINDOWS
            HANDLE_SIGNAL_TIME(stderr,nonterms,number,log_level,options,
                               statefile,Counter1,Counter2,CountIterations,
                               NumTime,NumIterations,remTime,StartTime,
                               StopTime,StartEstiTime,EndEstiTime,
                               estimate_time,best,best_poly,opt_nontermstate,
                               opt_nonterms,GrayCount,BinaryCount,chPos,Card,
                               OptCard,OptCount,UpdateCount,Tables,CurTable,
                               key)
#endif
          }
        while (chPos != number.opt);
      }

    // Find the table configuration that leads to the best polynomial runtime
    // (option "-g")
    else
      { int comp;
        do
          { // Update table configuration
            GrayCount[chPos] = 1 - GrayCount[chPos];
            nonterms[opt_nonterms[chPos]].tab = GrayCount[chPos];
            v = (struct nt_vertex*) graph_vertex(graph,opt_nonterms[chPos]+1);
            v->tabulated = 1 - GrayCount[chPos];
            if (GrayCount[chPos] == 0)
              Card += CardCount[chPos];
            else
              Card -= CardCount[chPos];
            // Calculate runtime and update solutions
            if (Card >= Card_lowerbound)
              { new_poly = compute_runtime(graph);
                // Better polynomial runtime ?
                if ((comp = poly_compare(best_poly,new_poly)) == 1)
                  { free(best_poly);
                    best_poly = new_poly;
                    NEW_BEST_TABLECONF(Tables,CurTable,GrayCount,number.opt,
                                       OptCount,OptCard,Card)
                    if (log_level >= log_verbose)
                      PRINT_UPDATE_INFO(stderr,UpdateCount,CountIterations,
                                        Counter1,OptCard,nonterms,
                                        number.total,new_poly)
                  }
                // Equal polynomial runtime ?
                else if (comp == 0)
                  { free(new_poly);
                    APPEND_TABLECONF(CurTable,GrayCount,number.opt,OptCount)
                    if (Card < OptCard)
                      OptCard = Card;
                    if (log_level >= log_verbose)
                      PRINT_UPDATE_INFO(stderr,UpdateCount,CountIterations,
                                        Counter1,OptCard,nonterms,
                                        number.total,best_poly)
                  }
                else
                  free(new_poly);
              }
            // Calculate next table configuration
            CODE_COUNT(BinaryCount,number.opt,chPos)
            // Handle signals and the runtime-estimation
#ifndef WINDOWS
            HANDLE_SIGNAL_TIME(stderr,nonterms,number,log_level,options,
                               statefile,ext_signal,new_action,Counter1,
                               Counter2,CountIterations,NumTime,NumIterations,
                               remTime,StartTime,StopTime,StartEstiTime,
                               EndEstiTime,estimate_time,best,best_poly,
                               opt_nontermstate,opt_nonterms,GrayCount,
                               BinaryCount,chPos,Card,OptCard,OptCount,
                               UpdateCount,Tables,CurTable)
#endif
#ifdef WINDOWS
            HANDLE_SIGNAL_TIME(stderr,nonterms,number,log_level,options,
                               statefile,Counter1,Counter2,CountIterations,
                               NumTime,NumIterations,remTime,StartTime,
                               StopTime,StartEstiTime,EndEstiTime,
                               estimate_time,best,best_poly,opt_nontermstate,
                               opt_nonterms,GrayCount,BinaryCount,chPos,Card,
                               OptCard,OptCount,UpdateCount,Tables,CurTable,
                               key)
#endif
          }
        while (chPos != number.opt);
      }
    
    
    // Remove the dummy entry in the list of the optimal table configurations
    if (!(Tables->td))
      { CurTable = Tables;
        Tables = Tables->next;
        free(CurTable);
      }       
    
    // Print the result
    if (log_level >= log_default)
      { // Print the actual used runtime
        fprintf(stderr, "\nReady! Actual runtime        : ");
        time_t EndTime = time(NULL);
        print_time(stderr, difftime(EndTime, StartTime));
        fprintf(stderr, "\n");
        // Print the result
        fprintf(stderr, "\nOptimal runtime              : %s\n",
                        asm2string(best));
        fprintf(stderr, "Minimum number of tables     : %d\n", OptCard);
        fprintf(stderr, "Optimal table configurations : %u\n", OptCount);
        print_asmopt_tableconf(stderr,nonterms,number,Tables,graph,
                               opt_nonterms,best);
        get_best_polynomial_runtime(nonterms,number,Tables,graph,opt_nonterms,
                                    &best_poly,&best_poly_count);
        if (best < EXPONENTIAL)
          { fprintf(stderr, "\nOptimal table configurations with respect ");
            fprintf(stderr, "to constant factors : %u\n", best_poly_count);
            print_polyopt_tableconf(stderr,nonterms,number,Tables,graph,
                                    opt_nonterms,best_poly);
          }
      }
    
    // Write the result-file
    if (options.resultfilename)
      { if (resfile)
          { write_resfile(resfile,nonterms,number,runtime,list,Tables,graph,
                          opt_nonterms,best,&options,OptCard,OptCount);
            fclose(resfile);
          }
        else
          fprintf(stderr, "\nError: Cannot create file \'%s\'\n\n",
                          options.resultfilename);
      }
    
    return 0;
  }
