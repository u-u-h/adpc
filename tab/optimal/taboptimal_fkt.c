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

/*! \file taboptimal_fkt.c

    \brief Auxiliary functions that are used by "taboptimal.c".

*/


#include <taboptimal_fkt.h>

/*! \brief The version of the program. */
#define PROGVERSION "1.0"


void handle_command_line_options (int argc, char** argv, const char* progname,
                                  const char* tabresult_filename,
                                  Opt_Rec* options)

  { char c, error = '\0';
    char* tailptr;
    Nt_List nt_list;
    // Set the default options
    options->print_dep_graph = 0;
    options->user = 1;
    options->red = 1;
    options->nobest = 0;
    options->globalbest = -1;
    options->square = 0;
    options->lin = 0;
    options->verbose = log_default;
    options->bound = -1;
    options->exact = -1;
    options->TableDef = 0;
    options->TableConf = NULL;
    options->time = ESTIMATE_TIME;
    options->statefilename = NULL;
    options->resultfilename = malloc((strlen(tabresult_filename)+1));
    strcpy(options->resultfilename,tabresult_filename);
    opterr = 0;
    while ((c = getopt(argc, argv, "hduflpnv:b:e:g:t:a:i:s:r:")) != -1)
      switch (c)
        { // option: help
          case 'h':
            print_help_text(stderr, progname, tabresult_filename);
            exit(0);
            break;
          // option: print dependencies (debug)
          case 'd':
            options->print_dep_graph = 1;
            break;
          // option: ignore user annotation
          case 'u':
            options->user = 0;
            break;
          // option: force square tables
          case 'f':
            options->square = 1;
            break;
          // option: linear and constant tables contribute
          case 'l':
            options->lin = 1;
            break;
          // option: no preprocessing reductions
          case 'p':
            options->red = 0;
            break;
          // option: do not use best possible runtime to prune the
          // search-space
          case 'n':
            if (options->exact == -1)
              { options->nobest = 1;
                options->globalbest = -1;
              }
            break;
          // option: specify verbosity level
          case 'v':
            options->verbose = (int) strtol(optarg, &tailptr, 10);
            if ((options->verbose < log_none) ||
                (options->verbose > log_verbose) ||
                (strlen(tailptr) > 0))
              error = 'v';
            break;
          // option: upper bound of the number of tables
          case 'b':
            options->bound = (int) strtol(optarg, &tailptr, 10);
            if ((options->bound < 0) ||
                (strlen(tailptr) > 0))
              error = 'b';
            options->exact = -1;
            options->globalbest = -1;
            break;
          // option: exact number of tables
          case 'e':
            options->exact = (int) strtol(optarg, &tailptr, 10);
            if ((options->exact < 0) ||
                (strlen(tailptr) > 0))
              error = 'e';
            options->bound = -1;
            options->nobest = 0;
            options->globalbest = -1;
            break;
          // option: global best polynomial runtime
          case 'g':
            options->globalbest = (int) strtol(optarg, &tailptr, 10);
            if ((options->globalbest < 0) ||
                (strlen(tailptr) > 0))
              error = 'g';
            options->exact = -1;
            options->bound = -1;
            options->nobest = 0;
            break;
          // option: specify table configuration
          case 't':
            options->TableDef = 1;
            options->exact = -1;
            options->bound = -1;
            options->nobest = 0;
            options->globalbest = -1;
            tailptr = optarg;
            while ((strlen(tailptr) > 0) && (error == '\0'))
              { nt_list = malloc(sizeof(struct nt_list));
                nt_list->nt = (int) strtol(tailptr, &tailptr, 10);
                // The Suffixes "l" and "c" denote linear and constant tables
                // and should not case an error. Furthermore ignore spaces at
                // the end of the input.
                while (tailptr &&
                      ((tailptr[0] == 'l') || (tailptr[0] == 'c') ||
                       (tailptr[0] == ' ')))
                  tailptr++;
                if (nt_list->nt <= 0)
                  error = 't';
                nt_list->next = options->TableConf;
                nt_list->prev = NULL;
                options->TableConf = nt_list;
              }
            break;
          // option: annotate nonterminals
          case 'a':
            options->TableDef = 2;
            tailptr = optarg;
            while ((strlen(tailptr) > 0) && (error == '\0'))
              { nt_list = malloc(sizeof(struct nt_list));
                nt_list->nt = (int) strtol(tailptr, &tailptr, 10);
                // The Suffixes "l" and "c" denote linear and constant tables
                // and should not case an error. Furthermore ignore spaces at
                // the end of the input.
                while (tailptr &&
                      ((tailptr[0] == 'l') || (tailptr[0] == 'c') ||
                       (tailptr[0] == ' ')))
                  tailptr++;
                // nt_list->nt < 0 has to be allowed because nonterminals that
                // are annotated nontabulated are represented by negative
                // values.
                if (nt_list->nt == 0)
                  error = 'u';
                nt_list->next = options->TableConf;
                nt_list->prev = NULL;
                options->TableConf = nt_list;
              }
            break;
          // options: Set time-interval of runtime-estimations
          case 'i':
            options->time = (int) strtol(optarg, &tailptr, 10);
            if (options->time < 1)
              error = 'i';
            break;
          // option: save program's execution state
          case 's':
            options->statefilename = optarg;
            break;
          // option: specify result-file
          case 'r':
            free(options->resultfilename);
            if (strcmp("/",optarg) == 0)
              options->resultfilename = NULL;
            else
              options->resultfilename = optarg;
            break;
          default : 
            error = (char) optopt;
            break;
        }
  
  if (error != '\0')
    { fprintf(stderr, "\nInvalid option parameter or invalid use of ");
      fprintf(stderr, "option \'-%c\'.\n", error);
      fprintf(stderr, "Use \'%s -h\' for help.\n\n", progname);
      exit(1);
    }  
  // Is there an input-filename given ?
  if (optind < argc)
    { options->inputfilename = malloc((strlen(argv[optind])+1));
      strcpy(options->inputfilename,argv[optind]);
    }
  else
    { options->inputfilename = NULL;
      fprintf(stderr, "\ntaboptimal version " PROGVERSION "\n");
      fprintf(stderr, "\nThis program calculates the optimal table ");
      fprintf(stderr, "configuration of an ADP-program.\n");
      fprintf(stderr, "\nUse \'%s -h\' for further information.\n\n",
                      progname);
      exit(0);
    }
  }


void print_help_text (FILE* file, const char* progname, 
                      const char* tab_resultfilename)

  { fprintf(file, "\ntaboptimal version " PROGVERSION " \n"
                  "\nThis program calculates the optimal table "
                  "configuration of an ADP-program.\n"
                  "\nUsage:  %s [Options] <adp-file>\n"
                  "\n<adp-file> contains the serialized adp-grammar"
                  ". Use \'-\' for stdin.\n"
                  "\nOptions:\n"
                  "  -h\tDisplay this help text.\n"
                  "  -d\tPrint some imformation about the internal "
                  "represantation of the\n\tnonterminals and parser "
                  "dependencies.\n"
                  "  -i k\tPrint a runtime-estimation every k seconds.\n"
                  "  -u\tIgnore user annotation.\n"
                  "  -f\tForce use of square tables for each nonterminal.\n"
                  "  -l\tEven linear and constant tables contribute to the "
                  "cardinality of the\n\ttable configuration (not "
                  "only square tables).\n"
                  "  -p\tDo not apply preprocessing reductions.\n"
                  "  -n\tDo not use the optimal runtime computed in advance "
                  "to prune the search\n\tspace. Compute the runtime for "
                  "every table configuration in the search\n\tspace.\n"
                  "  -vl\tSpecify verbosity level:\n"
                  "  \t  l = 0 : no output\n"
                  "  \t  l = 1 : default output\n"
                  "  \t  l = 2 : verbose output\n"
                  "  -b n\tAssume n to be an upper bound of the "
                  "number of tables in the optimal\n\ttable "
                  "configuration (This might be useful to speed up "
                  "the computation\n\tby pruning the search space, if you "
                  "already know an asymptotically\n\toptimal table "
                  "configuration that uses n tables).\n"
                  "  -e n\tOnly consider table configurations "
                  "with exact n tables.\n"
                  "  -g n\tCalculate the best possible polynomial runtime "
                  "for table\n\tconfigurations with >= n tables.\n"
                  "  -t \"n1 n2 ... nk\"\n"
                  "\tDefine the table configuration. The "
                  "nonterminals n1, n2, ..., nk are\n\ttabulated, "
                  "all others are recursive functions. The "
                  "program simply\n\tcomputes the runtime of this "
                  "table configuration.\n"
                  "  -a \"[+,-]n1 [+,-]n2 ... [+,-]nk\"\n"
                  "\tAnnotate the nonterminals n1, n2, ..., nk.\n"
                  "\t  +nk: tabulate nonterminal nk\n"
                  "\t  -nk: do not tabulate nonterminal nk\n"
                  "  -s <state-file>\n"
                  "\tWrite the program\'s state to <state-file> if "
                  "the program is aborted.\n\tIf <state-file> already "
                  "exists then the program\'s state is being read\n\t"
                  "from this file and the program\'s execution will be "
                  "continued.\n"
                  "  -r <result-file>\n"
                  "\tWrite the result to <result-file>. \'-\' "
                  "specifies stdout.\n\t\'%s\' "
                  "is used as default if the option \'-r\' is "
                  "omitted.\n"
                  "  -r/\tDo not write a resultfile.\n"
                  "\n", progname, tab_resultfilename
           );
  }


#ifndef WINDOWS
/*! \brief The signal-handler to handle some important signals.

    The only thing that the signal-handler does is to set two global variables
    which are checked by the main-program frequently and are handled there.

    This function is called by the system if a signal arrives.

    \param signum The id of the arriving signal
*/

void signal_handler (int signum)

  { extern volatile int ext_signal;
    extern volatile unsigned int NumTime;
    ext_signal = signum;
    NumTime = 0;
  }


void setup_signal_handler (Opt_Rec options)

  { struct sigaction new_action, old_action;
    new_action.sa_handler = signal_handler;
    sigemptyset(&new_action.sa_mask);
    new_action.sa_flags = 0;

    int error = 0;
    // signal TSTP
    sigaction(SIGTSTP, NULL, &old_action);
    if (old_action.sa_handler != SIG_IGN)
      sigaction(SIGTSTP, &new_action, NULL);
    else
      { error = 1;
        fprintf(stderr, "\nNote: Set up signal handler for signal "
                        "TSTP failed.\n");
      }
    // signal CONT
    sigaction(SIGCONT, NULL, &old_action);
    if (old_action.sa_handler != SIG_IGN)
      sigaction(SIGCONT, &new_action, NULL);
    else
      { error = 1;
        fprintf(stderr, "\nNote: Set up signal handler for signal "
                        "CONT failed.\n");
      }
    if (error)
      fprintf(stderr, "\nNote: Stopping the program may lead to a bad "
                      "runtime estimation.\n");
    error = 0;
    // Is the program started with option "-s" ?
    if (options.statefilename)
      { // signal INT
        sigaction(SIGINT, NULL, &old_action);
        if (old_action.sa_handler != SIG_IGN)
          sigaction(SIGINT, &new_action, NULL);
        else
          { error = 1;
            fprintf(stderr, "\nNote: Set up signal handler for signal "
                            "INT failed.\n");
          }
        // signal HUP
        sigaction(SIGHUP, NULL, &old_action);
        if (old_action.sa_handler != SIG_IGN)
          sigaction(SIGHUP, &new_action, NULL);
        else
          { error = 1;
            fprintf(stderr, "\nNote: Set up signal handler for signal "
                            "HUP failed.\n");
          }
        // signal TERM
        sigaction(SIGTERM, NULL, &old_action);
        if (old_action.sa_handler != SIG_IGN)
          sigaction(SIGTERM, &new_action, NULL);
        else
          { error = 1;
            fprintf(stderr, "\nNote: Set up signal handler for signal "
                            "TERM failed.\n");
          }
        // signal QUIT
        sigaction(SIGQUIT, NULL, &old_action);
        if (old_action.sa_handler != SIG_IGN)
          sigaction(SIGQUIT, &new_action, NULL);
        else
          { error = 1;
            fprintf(stderr, "\nNote: Set up signal handler for signal "
                            "QUIT failed.\n");
          }
        if (error)
          fprintf(stderr, "\nWriting a state-file if the program aborts "
                          "will not succed in every case.\n");
      }

  }
#endif

/*! \brief Compare two option-records and check whether they match.

    This function finishes the program if the two option-records do not match.

    \param options The first option-record
    \param Refoptions The second options-record
*/

static void check_options (Opt_Rec options, Opt_Rec Refoptions)

  { int error = 0;
    if (options.user != Refoptions.user)
      { error = 1;
        fprintf(stderr, "\nError: Command-line-options and settings in "
                        "\'%s\' do not match:\n       Option \'-u\'.\n",
                        options.statefilename);
      }
    if (options.red != Refoptions.red)
      { error = 1;
        fprintf(stderr, "\nError: Command-line-options and settings in "
                        "\'%s\' do not match:\n       Option \'-p\'.\n",
                        options.statefilename);
      }
    if (options.lin != Refoptions.lin)
      { error = 1;
        fprintf(stderr, "\nError: Command-line-options and settings in "
                        "\'%s\' do not match:\n       Option \'-l\'.\n",
                        options.statefilename);
      }
    if (options.square != Refoptions.square)
      { error = 1;
        fprintf(stderr, "\nError: Command-line-options and settings in "
                        "\'%s\' do not match:\n       Option \'-f\'.\n",
                        options.statefilename);
      }
    if (options.nobest != Refoptions.nobest)
      { error = 1;
        fprintf(stderr, "\nError: Command-line-options and settings in "
                        "\'%s\' do not match:\n       Option \'-n\'.\n",
                        options.statefilename);
      }
    if (options.bound != Refoptions.bound)
      { error = 1;
        fprintf(stderr, "\nError: Command-line-options and settings in "
                        "\'%s\' do not match:\n       Option \'-b\': "
                        "%d <-> %d\n", options.statefilename, options.bound,
                        Refoptions.bound);
      }
    if (options.exact != Refoptions.exact)
      { error = 1;
        fprintf(stderr, "\nError: Command-line-options and settings in "
                        "\'%s\' do not match:\n       Option \'-e\': "
                        "%d <-> %d\n", options.statefilename, options.exact,
                        Refoptions.exact);
      }
    if (options.globalbest != Refoptions.globalbest)
      { error = 1;
        fprintf(stderr, "\nError: Command-line-options and settings in "
                        "\'%s\' do not match:\n       Option \'-g\': "
                        "%d <-> %d\n", options.statefilename,
                        options.globalbest,Refoptions.globalbest);
      }
    
    if (error)
      { fprintf(stderr, "\nReading the program\'s execution state "
                        "from \'%s\' failed.\n\n", options.statefilename);
        exit(error);
      }
  }


/*! \brief Compare two nonterminal-number-records and check whether they 
    \brief match.
    
    This function finishes the program if the two nonterminal-number-records
    do not match.
    
    \param number The first nonterminal-number-record
    \param Refnumber The second nonterminal-number-record
    \param options Option-record. It is used to print the filenames to stderr
                   if the records do not match.
*/

static void check_numbers(Number_Info number, Number_Info Refnumber,
                          Opt_Rec options)

  { int error = 0;
    if ((number.total  != Refnumber.total ) ||
        (number.used   != Refnumber.used  ) ||
        (number.unused != Refnumber.unused) ||
        (number.user   != Refnumber.user  ) ||
        (number.red    != Refnumber.red   ) ||
        (number.opt    != Refnumber.opt   ))
      { error = 1;
        fprintf(stderr, "\nError: The structure of the nonterminals "
                        "does not match: \'%s\' <-> \'%s\'\n",
                        options.inputfilename, options.statefilename);
      }
    if (error)
      { fprintf(stderr, "\nReading the program\'s execution state "
                        "from \'%s\' failed.\n\n", options.statefilename);
        exit(error);
      }
 }


void write_state_file (FILE* statefile, int log_level, Opt_Rec options,
                       Number_Info number, int* opt_nontermstate,
                       char* GrayCount, char* BinaryCount, int chPos,
                       int Card, int OptCard, unsigned int OptCount,
                       unsigned int UpdateCount, int best, Poly best_poly,
                       TableDesign_List Tables, unsigned int Counter1,
                       unsigned int Counter2, double CountIterations,
                       time_t StartTime, time_t StopTime)

  { int i;
    if (!statefile)
      { fprintf(stderr, "\nError: Cannot create file \'%s\'\n",
                        options.statefilename);
        fprintf(stderr, "\nTry to write the execution state to \'%s\' "
                        "instead.\n", STATE_FILENAME);
        statefile = fopen(STATE_FILENAME, "w");
        options.statefilename = malloc(strlen(STATE_FILENAME)+1);
        strcpy(options.statefilename,STATE_FILENAME);
      }
    if (statefile)
      { if (!(Tables->td))
          Tables = Tables->next;
        fprintf(statefile, "Taboptimal execution state file.\n"
                           "Please do not change anything in this file.\n"
                           "(not even comments!)\n\n");
        fprintf(statefile, "-numbers\n");
        fprintf(statefile, "%d;%d;%d;%d;%d;%d;\n",
                           number.total,number.used,number.unused,number.user,
                           number.red,number.opt);
        fprintf(statefile, "-options\n");
        fprintf(statefile, "%d;%d;%d;%d;%d;%d;%d;%d;\n",
                           options.user,options.red,options.nobest,
                           options.lin,options.square,options.bound,
                           options.exact,options.globalbest);
        fprintf(statefile, "-scalar values\n");
        fprintf(statefile, "%d;%d;%d;%d;",
                           chPos,Card,OptCard,best);
        fprintf(statefile, "%u;%u;%u;%u;%.0lf;\n",
                           OptCount,UpdateCount,Counter1,Counter2,
                           CountIterations);
        fprintf(statefile, "-time\n");
        fprintf(statefile, "%.0lf;%.0lf;\n",
                           (double) StartTime, (double) StopTime);
        fprintf(statefile, "-opt_nontermstate\n");
        for (i = 0; i < number.total; i++)
          fprintf(statefile, "%d;", opt_nontermstate[i]);
        fprintf(statefile, "\n");
        fprintf(statefile, "-GrayCount\n");
        for (i = 0; i < number.opt; i++)
          fprintf(statefile, "%d;", (int) GrayCount[i]);
        fprintf(statefile, "\n");
        fprintf(statefile, "-BinaryCount\n");
        for (i = 0; i < number.opt; i++)
          fprintf(statefile, "%d;", (int) BinaryCount[i]);
        fprintf(statefile, "\n");
        fprintf(statefile, "-Tables\n");
        while (Tables)
          { for (i = 0; i < number.opt; i++)
              fprintf(statefile, "%d;", (int) (Tables->td)[i]);
            fprintf(statefile, "\n");
            Tables = Tables->next;
          }
        if (options.globalbest > -1)
          { fprintf(statefile, "-Best polynomial\n");
            fprintf(statefile, "%" PRIfactor ";", best_poly[0]);
            for (i = 1; i <= best_poly[0]+1; i++)
              fprintf(statefile, "%" PRIfactor ";", best_poly[i]);
            fprintf(statefile, "\n");
          }  
        fclose(statefile);
        if (log_level >= log_default)
          fprintf(stderr, "\nThe program\'s execution state has been "
                          "successfully written to \'%s\'.\n",
                          options.statefilename);
      }
    else
      { fprintf(stderr, "\nError: Writing state-file failed. The information "
                        "about the program\'s execution state got lost.\n");
      }
  }
  

/*! \brief Read the program's execution state from a stream.
    
    The parameters correspond to the equal-named variables in the mainloop
    of the brute-force-optimiation - see "taboptimal.c".

    \param statefile Source-stream
    \param log_level Verbosity level (see "asm_typedefs.h")
    \param options Option-record (RESULT)
    \param number Nonterminal-number-record (RESULT)
    \param opt_nontermstate The optimization-state of the nonterminals. See
                            the description in "taboptimal.c" and 
                            "set_opt_nonterms". (RESULT)
    \param GrayCount The table configuration of the optimization-nonterminals
                     that is considered currently - in Graycode-representation
                     (RESULT)
    \param BinaryCount The table configuration of the optimization-
                       nonterminals that is considered currently - in 
                       Binarycode-representation (RESULT)
    \param chPos The position of the last tabulation change (see
                 "taboptimal.c") (RESULT)
    \param Card The cardinality of the table configuration considered
                currently (RESULT)
    \param OptCard The cardinality of the best table configuration found so
                   far (RESULT)
    \param OptCount The number of the optimal table configurations found so
                    far(RESULT)
    \param UpdateCount The number of solution updates so far (RESULT)
    \param best Optimal asymptotical runtime (RESULT)
    \param best_poly The best polynomial runtime found so far. An appropriate
                     amount of memory has to be allocated. (RESULT)
    \param Tables The List of the optimal table configurations found so far
                  (RESULT)
    \param Counter1 Executed iterations since the last time-check. (See
                    "taboptimal.c") (RESULT)
    \param Counter2 The number of time-checks since the last runtime-
                    estimation. (See "taboptimal.c") (RESULT)
    \param CountIterations The number of executed iterations from the start of
                           the brute-force-optimization to the last time-check
                           (see "taboptimal.c"). (RESULT)
    \param StartTime Starttime of the brute-force-optimization (RESULT)
    \param StopTime Time when the brute-force-optimization was interrupted
                    (RESULT)
    \param Refnumber The nonterminal-number-record to compare it with
                     "number". It has to match, otherwise an error will occur.
    \param Refoptions The program's options-record to compare it with
                      "options". It has to match, otherwise an error will
                      occur.
*/

static void read_state_file (FILE* statefile, int log_level, Opt_Rec* options,
                             Number_Info* number, int* opt_nontermstate,
                             char* GrayCount, char* BinaryCount, int* chPos,
                             int* Card, int* OptCard, unsigned int* OptCount,
                             unsigned int* UpdateCount, int* best,
                             Poly* best_poly, TableDesign_List* Tables,
                             unsigned int* Counter1, unsigned int* Counter2,
                             double* CountIterations, time_t* StartTime,
                             time_t* StopTime, Number_Info Refnumber,
                             Opt_Rec Refoptions)

  { int i, x;
    if (!statefile)
      { fprintf(stderr, "\nError: Cannot read data from \'%s\'.\n\n",
                        options->statefilename);
        exit(1);
      }
    else
      { number->total = 0;
        number->opt = 0;
        *OptCount = 0;
        fscanf(statefile, "Taboptimal execution state file.\n"
                          "Please do not change anything in this file.\n"
                          "(not even comments!)\n\n");
        fscanf(statefile, "-numbers\n");
        fscanf(statefile, "%d;%d;%d;%d;%d;%d;\n",
                          &(number->total),&(number->used),&(number->unused),
                          &(number->user),&(number->red),&(number->opt));
        fscanf(statefile, "-options\n");
        fscanf(statefile, "%d;%d;%d;%d;%d;%d;%d;%d;\n",
                          &(options->user),&(options->red),&(options->nobest),
                          &(options->lin),&(options->square),
                          &(options->bound),&(options->exact),
                          &(options->globalbest));
        fscanf(statefile, "-scalar values\n");
        fscanf(statefile, "%d;%d;%d;%d;",
                          chPos,Card,OptCard,best);
        fscanf(statefile, "%u;%u;%u;%u;%lf;\n",
                          OptCount,UpdateCount,Counter1,Counter2,
                          CountIterations);
        double d_StartTime, d_StopTime;
        fscanf(statefile, "-time\n");
        fscanf(statefile, "%lf;%lf;\n",
                          &d_StartTime, &d_StopTime);
        *StartTime = (time_t) d_StartTime;
        *StopTime = (time_t) d_StopTime;
        fscanf(statefile, "-opt_nontermstate\n");
        for (i = 0; (i < number->total) && (i < Refnumber.total); i++)
          fscanf(statefile, "%d;", &(opt_nontermstate[i]));
        fscanf(statefile, "\n");
        fscanf(statefile, "-GrayCount\n");
        for (i = 0; (i < number->opt) && (i < Refnumber.opt); i++)
          { fscanf(statefile, "%d;", &x);
            GrayCount[i] = (char) x;
          }
        fscanf(statefile, "\n");
        fscanf(statefile, "-BinaryCount\n");
        for (i = 0; (i < number->opt) && (i < Refnumber.opt); i++)
          { fscanf(statefile, "%d;", &x);
            BinaryCount[i] = (char) x;
          }
        fscanf(statefile, "\n");
        int j;
        *Tables = malloc(sizeof(struct tabledesign_list));
        TableDesign_List CurTable = *Tables;
        CurTable->td = NULL;
        CurTable->next = NULL;
        fscanf(statefile, "-Tables\n");
        for (j = 1; j <= *OptCount; j++)
          { CurTable->next = malloc(sizeof(struct tabledesign_list));
            CurTable = CurTable->next;
            CurTable->td = malloc(Refnumber.opt);
            CurTable->next = NULL;
            for (i = 0; (i < number->opt) && (i < Refnumber.opt); i++)
              { fscanf(statefile, "%d;", &x);
                (CurTable->td)[i] = (char) x;
              }
            fscanf(statefile, "\n");
          }
        if ((!((*Tables)->td)) && (*Tables != CurTable))
          { CurTable = *Tables;
            *Tables = CurTable->next;
            free(CurTable);
          }
        if (options->globalbest > -1)
          { fscanf(statefile, "-Best polynomial\n");
            fscanf(statefile, "%" PRIfactor ";", &((*best_poly)[0]));
            for (i = 1; i <= (*best_poly)[0]+1; i++)
              fscanf(statefile, "%" PRIfactor ";", &((*best_poly)[i]));
            fscanf(statefile, "\n");
          }  

        check_options(*options,Refoptions);
        check_numbers(*number,Refnumber,Refoptions);
        if (!feof(statefile))
          { fprintf(stderr, "\nError: Invalid file format\n");
            fprintf(stderr, "\nReading the program\'s execution state "
                            "from \'%s\' failed.\n\n",
                            options->statefilename);
            exit(1);
          } 
        fclose(statefile);
      }
  }


void get_execution_state (FILE* statefile, int log_level, Opt_Rec* options,
                          Number_Info* number, int* opt_nontermstate,
                          char* GrayCount, char* BinaryCount, int* chPos,
                          int* Card, int* OptCard, unsigned int* OptCount,
                          unsigned int* UpdateCount, int* best,
                          Poly* best_poly, TableDesign_List* Tables,
                          TableDesign_List* CurTable, unsigned int* Counter1,
                          unsigned int* Counter2, double* CountIterations,
                          time_t* StartTime, time_t* StopTime, 
                          time_t* StartEstiTime, Asm_Nts nonterms,
                          int* opt_nonterms, Runtime_Info runtime)
                          
  { // Variables with the prefix "R_" are to be read from the state-file.
    // Afterwards they are compared to the corresponding variables without
    // prefix to check whether they match.
    Opt_Rec R_options;
    Number_Info R_number;
    int* R_opt_nontermstate = calloc(number->total, sizeof(int));
    int R_best, i, error = 0;
    R_options.statefilename = options->statefilename;
    read_state_file(statefile,log_level,&R_options,&R_number,
                    R_opt_nontermstate,GrayCount,BinaryCount,chPos,Card,
                    OptCard,OptCount,UpdateCount,&R_best,best_poly,Tables,
                    Counter1,Counter2,CountIterations,StartTime,StopTime,
                    *number,*options);
    
    // The option-records and the nonterminal-number-records match - this has
    // been tested in "read_state_file". It remains to check "best" and
    // "opt_nontermstate".
    for (i = 0;
         (i < number->total) && (opt_nontermstate[i] == R_opt_nontermstate[i]);
         i++);
      if ((i < number->total) || 
          ((R_best != runtime.best_user) && 
           (options->bound == -1) && (options->exact == -1)))
        { error = 1;
          fprintf(stderr, "\nError: The structure of the nonterminals "
                          "does not match: \'%s\' <-> \'%s\'\n",
                          options->inputfilename, options->statefilename);
          fprintf(stderr, "\nReading the program\'s execution state "
                          "from \'%s\' failed.\n\n", options->statefilename);
        }
      
    *best = R_best;
         
    // Set "*Curtable" to the correct value.
    *CurTable = *Tables;
    while ((*CurTable)->next)
      *CurTable = (*CurTable)->next;
    
    // Readjust the runtime-estimation
    *StartTime += time(NULL) - *StopTime;
    *StartEstiTime = 0;
    
    // Set the tabulation-state of the optimization-nonterminals
    for (i = 0; i < number->opt; i++)
      nonterms[opt_nonterms[i]].tab = GrayCount[i];
  
    if (log_level >= log_default)
      fprintf(stderr, "\nThe program\'s execution state has been "
                      "successfully read from \'%s\'.\n",
                      options->statefilename);
  }
