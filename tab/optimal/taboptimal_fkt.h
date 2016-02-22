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

/*! \file taboptimal_fkt.h

    \brief Auxiliary functions that are used by "taboptimal.c".

*/


#ifndef _TABOPTIMAL_FKT_H_
#define _TABOPTIMAL_FKT_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include <signal.h>
#include <unistd.h>

#include <tabulate.h>
#include <taboptimal.h>
#include <asm_typedefs.h>
#include <asm_runtime.h>
#include <functions.h>
#include <input.h>
#include <output.h>
#include <poly_runtime.h>

#ifdef WINDOWS
/* conio.h and the conio-library are neccessary to handle the termination of
   the program by pressing ESCAPE. This files are not standard, but part of
   the MinGW-Package, for instance.
*/
#include <conio.h>
#endif


/*! \brief Read the commandline-options and either handle them directly or
    \brief return them to the calling function.
    \param argc The number of entries in the commandline. It is the same
                parameter as in the main-function.
    \param argv The entries in the commandline. It is the same parameter as in
                the main-function.
    \param progname The name of this program (only to display it)
    \param tabresult_filename The default name of the result-file (to display
                              it and to use it as default value in the
                              corresponding entry in the options-record).
    \param options The options-record to store the program-options (RESULT)
*/

void handle_command_line_options (int argc, char** argv, const char* progname,
                                  const char* tabresult_filename,
                                  Opt_Rec* options);


/*! \brief Print a short description about the program and the available
    \brief options to a stream.
    \param file Target-stream
    \param progname The name of this program (only to display it)
    \param tab_resultfilename The default name of the result-file (only to
                              display it)
*/

void print_help_text (FILE* file, const char* progname,
                      const char* tab_resultfilename);


#ifndef WINDOWS
/*! \brief Set up a signal-handler that handles some important signals.

    The signals TSTP and CONT are always handled. This is used to readjust the
    runtime-estimation if the program has been stopped and is continued later.
    Otherwise the runtime-estimation would be bad because the program would
    not know that it has been stopped and would think that it was very slow.

    The signals INT, HUP, TERM and QUIT are handled only if the program has
    been started with commandline-option "-s". This is used to save the
    program's execution state before terminating.

    This signal-handler handles all these signals.

    \param options Option-record (only to test whether options "-s" is used)
*/

void setup_signal_handler (Opt_Rec options);
#endif


/*! \brief Write the program's execution state to a stream.

    The parameters correspond to the equally named variables in the mainloop
    of the brute-force-optimiation - see "taboptimal.c".

    \param statefile Target-stream
    \param log_level Verbosity level (see "asm_typedefs.h")
    \param options Option-record
    \param number Nonterminal-number-record
    \param opt_nontermstate The optimization-state of the nonterminals. See
                            the description in "taboptimal.c" and
                            "set_opt_nonterms".
    \param GrayCount The table configuration of the optimization-nonterminals
                     that is considered currently - in Graycode-representation
    \param BinaryCount The table configuration of the optimization-
                       nonterminals that is considered currently - in
                       Binarycode-representation
    \param chPos The position of the last tabulation change (see
                 "taboptimal.c")
    \param Card The cardinality of the table configuration considered
                currently
    \param OptCard The cardinality of the best table configuration found so
                   far
    \param OptCount The number of the optimal table configurations found so
                    far
    \param UpdateCount The number of solution updates so far
    \param best Optimal asymptotical runtime
    \param best_poly The best polynomial runtime found so far.
    \param Tables The List of the optimal table configurations found so far
    \param Counter1 Executed iterations since the last time-check. (See
                    "taboptimal.c")
    \param Counter2 The number of time-checks since the last runtime-
                    estimation. (See "taboptimal.c")
    \param CountIterations The number of executed iterations from the start of
                           the brute-force-optimization to the last time-check
                           (see "taboptimal.c").
    \param StartTime Starttime of the brute-force-optimization
    \param StopTime Time when the brute-force-optimization was interrupted
*/

void write_state_file (FILE* statefile, int log_level, Opt_Rec options,
                       Number_Info number, int* opt_nontermstate,
                       char* GrayCount, char* BinaryCount, int chPos,
                       int Card, int OptCard, unsigned int OptCount,
                       unsigned int UpdateCount, int best, Poly best_poly,
                       TableDesign_List Tables, unsigned int Counter1,
                       unsigned int Counter2, double CountIterations,
                       time_t StartTime, time_t StopTime);


/*! \brief Get the program's execution state and prepare to continue the
    \brief execution of the brute-force-optimization.

    The parameters correspond to the equal-named variables in the mainloop
    of the brute-force-optimization - see "taboptimal.c". They are read from
    a stream and it is checked whether they match the information known from
    the grammar-file and the program-options.

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
                    far (RESULT)
    \param UpdateCount The number of solution updates so far (RESULT)
    \param best Optimal asymptotical runtime (RESULT)
    \param best_poly The best polynomial runtime found so far. An appropriate
                     amount of memory has to be allocated. (RESULT)
    \param Tables The list of the optimal table configurations found so far
                  (RESULT)
    \param CurTable Pointer to the last added entry in the list of table
                    configurations (RESULT)
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
    \param StartEstiTime Time of the last runtime-estimation (RESULT)
    \param nonterms Nonterminals. The tab-fields are modified according to the
                    table configuration that is considered currently.
    \param opt_nonterms Optimization-nonterminals - see the description in
                        "taboptimal.c" and "set_opt_nonterms"
    \param runtime Runtime-record (to check whether it matches "best")
*/

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
                          int* opt_nonterms, Runtime_Info runtime);


/*! \brief Handle the estimation of the remaining runtime

    This macro is called by HANDLE_SIGNAL_TIME.

    The parameters correspond to the equally named variables in the mainloop
    of the brute-force-optimization - see "taboptimal.c".

    We use a macro instead of a function, because this code is executed in
    every iteration of the brute-force-optimization. Even an inline-function
    might be slower.

    Some of the parameters will be modified.

    This macro uses multiline-comments inside!

    \param file Target-stream
    \param log_level Verbosity level (see "asm_typedefs.h")
    \param remTime Estimated remaining runtime
    \param NumIterations Total number of iterations
    \param CountIterations The number of executed iterations from the start of
                           the brute-force-optimization to the last time-check
                           (see "taboptimal.c").
    \param Counter1 Executed iterations since the last time-check. (See
                    "taboptimal.c")
    \param Counter2 The number of time-checks since the last runtime-
                    estimation. (See "taboptimal.c")
    \param StartTime Starttime of the brute-force-optimization
    \param StopTime Time when the brute-force-optimization was interrupted
    \param EndEstiTime Time of the last time-check
    \param estimate_time Length of the timeinterval between two runtime-
                         estimations (in seconds)
    \param nonterms Nonterminals
    \param opt_nonterms Optimization-nonterminals - see the description in
                        "taboptimal.c" and "set_opt_nonterms"
    \param number Nonterminal-number-record
    \param OptCard The cardinality of the best table configuration found so
                   far
    \param CurTable Pointer to the last added entry in the list of table
                    configurations
    \param GrayCount The table configuration of the optimization-nonterminals
                     that is considered currently - in Graycode-representation
    \param options Option-record
    \param best_poly Best polynomial runtime found so far
*/

#define HANDLE_TIME(file, log_level, remTime, NumIterations,                 \
                    CountIterations, Counter1, Counter2, StartTime,          \
                    StopTime, EndEstiTime, estimate_time, nonterms,          \
                    opt_nonterms, number, OptCard, CurTable, GrayCount,      \
                    options, best_poly)                                      \
                                                                             \
  { EndEstiTime = time(NULL);                                                \
    CountIterations += Counter1;                                             \
    Counter1 = 0;                                                            \
    /* Is it time for a new runtime-estimation ? */                          \
    if ((log_level >= log_default) &&                                        \
        (difftime(EndEstiTime, StartEstiTime) >= estimate_time))             \
      { /* Calculate the estimated remaining runtime */                      \
        remTime = (NumIterations/CountIterations) *                          \
          difftime(EndEstiTime, StartTime)                                   \
            - difftime(EndEstiTime, StartTime);                              \
        fprintf(file, "\nCurrent iteration           : ");                   \
        fprintf(file, "%.0lf/%.0lf => %.2lf %%\n", CountIterations,          \
                        NumIterations,                                       \
                        (CountIterations/NumIterations*100));                \
        fprintf(file, "Runtime so far              : ");                     \
        print_time(file, difftime(EndEstiTime,StartTime));                   \
        fprintf(file, "\n");                                                 \
        fprintf(file, "Estimated remaining runtime : ");                     \
        print_time(file, remTime);                                           \
        fprintf(file, "\n");                                                 \
        fprintf(file, "One intermediate solution   : %d Tables\n",           \
                      OptCard);                                              \
        if (CurTable->td)                                                    \
          { update_table_configuration(nonterms,number.opt,                  \
                                       CurTable->td,opt_nonterms);           \
            fprintf(file, "  ");                                             \
            print_table_configuration(file,nonterms,number.total);           \
            fprintf(file, "\n");                                             \
            update_table_configuration(nonterms,number.opt,GrayCount,        \
                                       opt_nonterms);                        \
          }                                                                  \
        else                                                                 \
          fprintf(file, "   No solution found so far.\n");                   \
        if (options.globalbest > -1)                                         \
          { fprintf(file, "Polynomial runtime : ");                          \
            poly_print(file, best_poly);                                     \
            fprintf(file, "\n");                                             \
          }                                                                  \
        /* Adjust the frequency of time-checks */                            \
        if (Counter2 > 4)                                                    \
          NumTime *= 2;                                                      \
        else if (Counter2 < 2)                                               \
          NumTime /= 2;                                                      \
        /* If the execution was contiuned after stop or termination */       \
        /* then NumTime has been cleared and needs to be            */       \
        /* reinitialized                                            */       \
        if (NumTime == 0)                                                    \
          NumTime = INIT_NUMTIME;                                            \
        /* Update StopTime for the case that the program is not     */       \
        /* stopped by SIGTSTP but by SIGSTOP. Otherwise the         */       \
        /* readjustment of the runtime-estimation will not work     */       \
        /* in the latter case.                                      */       \
        StopTime = EndEstiTime;                                              \
        /* Update StartEstiTime */                                           \
        StartEstiTime = EndEstiTime;                                         \
        Counter2 = 0;                                                        \
      }                                                                      \
    else Counter2++;                                                         \
  }                                                                          \


/*** Begin of the Unix/Linux definition of HANDLE_SIGNAL_TIME ***************/
#ifndef WINDOWS

/*! \brief Handle the signals and estimate the remaining runtime during the
    \brief execution of the brute-force-optimization.

    The parameters correspond to the equally named variables in the mainloop
    of the brute-force-optimization - see "taboptimal.c".

    We use a macro instead of a function, because this code is executed in
    every iteration of the brute-force-optimization. Even an inline-function
    might be slower.

    Some of the parameters will be modified.

    This macro uses multiline-comments inside!

    \param file Target-stream
    \param nonterms Nonterminals
    \param number Nonterminal-number-record
    \param log_level Verbosity level (see "asm_typedefs.h")
    \param options Option-record
    \param statefile The file to write the program's execution state into
    \param ext_signal The signal which is set by the signal-handler - see
                      "taboptimal.h"
    \param new_action A signal-action-record. It is declared in the main-
                      program and locally used in this macro only.
    \param Counter1 Executed iterations since the last time-check. (See
                    "taboptimal.c")
    \param Counter2 The number of time-checks since the last runtime-
                    estimation. (See "taboptimal.c")
    \param CountIterations The number of executed iterations from the start of
                           the brute-force-optimization to the last time-check
                           (see "taboptimal.c").
    \param NumTime The number of iterations after which a new time-check is
                   performed. It is also used if a signal arrives. See
                   "taboptimal.c".
    \param NumIterations Total number of iterations
    \param remTime Estimated remaining runtime
    \param StartTime Starttime of the brute-force-optimization
    \param StopTime Time when the brute-force-optimization was interrupted
    \param StartEstiTime Time of the last runtime-estimation
    \param EndEstiTime Time of the last time-check
    \param estimate_time Length of the timeinterval between two runtime-
                         estimations (in seconds)
    \param best Optimal asymptotical runtime
    \param best_poly Best polynomial runtime found so far
    \param opt_nontermstate The optimization-state of the nonterminals. See
                            the description in "taboptimal.c" and
                            "set_opt_nonterms". (RESULT)
    \param opt_nonterms Optimization-nonterminals - see the description in
                        "taboptimal.c" and "set_opt_nonterms"
    \param GrayCount The table configuration of the optimization-nonterminals
                     that is considered currently - in Graycode-representation
    \param BinaryCount The table configuration of the optimization-
                       nonterminals that is considered currently - in
                       Binarycode-representation
    \param chPos The position of the last tabulation change (see
                 "taboptimal.c")
    \param Card The cardinality of the table configuration considered
                currently
    \param OptCard The cardinality of the best table configuration found so
                   far
    \param OptCount The number of the optimal table configurations found so
                    far
    \param UpdateCount The number of solution updates so far
    \param Tables The List of the optimal table configurations found so far
    \param CurTable Pointer to the last added entry in the list of table
                    configurations
*/

#define HANDLE_SIGNAL_TIME(file, nonterms, number, log_level, options,       \
                           statefile, ext_signal, new_action, Counter1,      \
                           Counter2, CountIterations, NumTime,               \
                           NumIterations, remTime, StartTime, StopTime,      \
                           StartEstiTime, EndEstiTime, estimate_time, best,  \
                           best_poly, opt_nontermstate, opt_nonterms,        \
                           GrayCount, BinaryCount, chPos, Card, OptCard,     \
                           OptCount, UpdateCount, Tables, CurTable)          \
                                                                             \
  { Counter1++;                                                              \
    if (Counter1 >= NumTime)                                                 \
      { if (ext_signal)                                                      \
          { /* Reset the default signal-handler */                           \
            new_action.sa_handler = SIG_DFL;                                 \
            sigemptyset(&new_action.sa_mask);                                \
            new_action.sa_flags = 0;                                         \
            sigaction(ext_signal,&new_action,NULL);                          \
            /* handle the signal */                                          \
            switch (ext_signal)                                              \
              { case SIGTSTP:                                                \
                  StopTime = time(NULL);                                     \
                  if (log_level >= log_default)                              \
                    fprintf(file, "\nProgram execution has been "            \
                                    "stopped.\n");                           \
                  ext_signal = 0;                                            \
                  raise(SIGTSTP);                                            \
                  break;                                                     \
                case SIGCONT:                                                \
                  /* Readjust runtime-estimation */                          \
                  StartTime += time(NULL) - StopTime;                        \
                  StartEstiTime = 0;                                         \
                  if (log_level >= log_default)                              \
                    fprintf(file, "\nContinue program execution. "           \
                                    "Runtime estimation readjusted.\n");     \
                  ext_signal = 0;                                            \
                  setup_signal_handler(options);                             \
                  break;                                                     \
                case SIGINT:                                                 \
                case SIGHUP:                                                 \
                case SIGTERM:                                                \
                case SIGQUIT:                                                \
                  StopTime = time(NULL);                                     \
                  statefile = fopen(options.statefilename, "w");             \
                  write_state_file(statefile,log_level,options,number,       \
                                   opt_nontermstate,GrayCount,BinaryCount,   \
                                   chPos,Card,OptCard,OptCount,UpdateCount,  \
                                   best,best_poly,Tables,Counter1,Counter2,  \
                                   CountIterations,StartTime,StopTime);      \
                  raise(ext_signal);                                         \
                  exit(0); /* if raise(ext_signal) fails - which should */   \
                           /* not happen                                */   \
                  break;                                                     \
              }                                                              \
          }                                                                  \
                                                                             \
        /* check the time and update counters */                             \
        HANDLE_TIME(file,log_level,remTime,NumIterations,CountIterations,    \
                    Counter1,Counter2,StartTime,StopTime,EndEstiTime,        \
                    estimate_time,nonterms,opt_nonterms,number,OptCard,      \
                    CurTable,GrayCount,options,best_poly)                    \
      }                                                                      \
  }

#endif
/*** End of the Unix/Linux definition of HANDLE_SIGNAL_TIME *****************/


/*** Begin of the Windows definition of HANDLE_SIGNAL_TIME ******************/
#ifdef WINDOWS

/*! \brief Save the execution state if the program is aborted and estimate the
    \brief remaining runtime during the execution of the brute-force-
    \brief optimization.

    The parameters correspond to the equally named variables in the mainloop
    of the brute-force-optimization - see "taboptimal.c".

    We use a macro instead of a function, because this code is executed in
    every iteration of the brute-force-optimization. Even an inline-function
    might be slower.

    Some of the parameters will be modified.

    This macro uses multiline-comments inside!

    \param file Target-stream
    \param nonterms Nonterminals
    \param number Nonterminal-number-record
    \param log_level Verbosity level (see "asm_typedefs.h")
    \param options Option-record
    \param statefile The file to write the program's execution state into
    \param Counter1 Executed iterations since the last time-check. (See
                    "taboptimal.c")
    \param Counter2 The number of time-checks since the last runtime-
                    estimation. (See "taboptimal.c")
    \param CountIterations The number of executed iterations from the start of
                           the brute-force-optimization to the last time-check
                           (see "taboptimal.c").
    \param NumTime The number of iterations after which a new time-check is
                   performed. It is also used if a signal arrives. See
                   "taboptimal.c".
    \param NumIterations Total number of iterations
    \param remTime Estimated remaining runtime
    \param StartTime Starttime of the brute-force-optimization
    \param StopTime Time when the brute-force-optimization was interrupted
    \param StartEstiTime Time of the last runtime-estimation
    \param EndEstiTime Time of the last time-check
    \param estimate_time Length of the timeinterval between two runtime-
                         estimations (in seconds)
    \param best Optimal asymptotical runtime
    \param best_poly Best polynomial runtime found so far
    \param opt_nontermstate The optimization-state of the nonterminals. See
                            the description in "taboptimal.c" and
                            "set_opt_nonterms". (RESULT)
    \param opt_nonterms Optimization-nonterminals - see the description in
                        "taboptimal.c" and "set_opt_nonterms"
    \param GrayCount The table configuration of the optimization-nonterminals
                     that is considered currently - in Graycode-representation
    \param BinaryCount The table configuration of the optimization-
                       nonterminals that is considered currently - in
                       Binarycode-representation
    \param chPos The position of the last tabulation change (see
                 "taboptimal.c")
    \param Card The cardinality of the table configuration considered
                currently
    \param OptCard The cardinality of the best table configuration found so
                   far
    \param OptCount The number of the optimal table configurations found so
                    far
    \param UpdateCount The number of solution updates so far
    \param Tables The List of the optimal table configurations found so far
    \param CurTable Pointer to the last added entry in the list of table
                    configurations
    \param key A int-variable that is used as a "local" variable of this
           macro. It set it to the ASCII-Code of the last pressed key.
*/

#define HANDLE_SIGNAL_TIME(file, nonterms, number, log_level, options,       \
                           statefile, Counter1, Counter2, CountIterations,   \
                           NumTime, NumIterations, remTime, StartTime,       \
                           StopTime, StartEstiTime, EndEstiTime,             \
                           estimate_time, best, best_poly, opt_nontermstate, \
                           opt_nonterms, GrayCount, BinaryCount, chPos,      \
                           Card, OptCard, OptCount, UpdateCount, Tables,     \
                           CurTable, key)                                    \
                                                                             \
  { Counter1++;                                                              \
    if (Counter1 >= NumTime)                                                 \
      { /* Is the program to terminate? */                                   \
        if (options.statefilename && kbhit())                                \
          { do  /* conio.h: Did the user pressed a key? */                   \
              { key = getch();  /* conio.h: get the ASCII-Code */            \
              }                                                              \
            while ((key != 27) && (kbhit()));                                \
            if (key == 27)  /* The ESCAPE-Key was pressed */                 \
              { /* Save the program's execution state */                     \
                StopTime = time(NULL);                                       \
                statefile = fopen(options.statefilename, "w");               \
                write_state_file(statefile,log_level,options,number,         \
                                 opt_nontermstate,GrayCount,BinaryCount,     \
                                 chPos,Card,OptCard,OptCount,UpdateCount,    \
                                 best,best_poly,Tables,Counter1,Counter2,    \
                                 CountIterations,StartTime,StopTime);        \
                exit(0);                                                     \
              }                                                              \
          }                                                                  \
                                                                             \
        /* check the time and update counters */                             \
        HANDLE_TIME(file,log_level,remTime,NumIterations,CountIterations,    \
                    Counter1,Counter2,StartTime,StopTime,EndEstiTime,        \
                    estimate_time,nonterms,opt_nonterms,number,OptCard,      \
                    CurTable,GrayCount,options,best_poly)                    \
      }                                                                      \
  }

#endif
/*** End of the Windows definition of HANDLE_SIGNAL_TIME ********************/


#endif
