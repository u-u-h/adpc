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

/*! \file asm_typedefs.h

    \brief Datastructures for the asymptotical runtime calculation. 

    To represent an asymptotical runtime and an asymptotical number of calls
    or usages we simply use the int-datatype. The representation is as
    follows:
      
      i = 0            <->  1
      i = 1            <->  O(1)
      i = 2            <->  O(n)
      i = 3            <->  O(n^2)
                       ...
      i = k            <->  O(n^(k-1))
                       ...
      i = EXPONENTIAL  <-> O(2^n)  [ = O(n^EXPONENTIAL) ]
    
    We differ between 1 and O(1) because in case of cycles in the dependence-
    graph we need to know whether the accumulated usages/calls equals 1 or are
    greater than 1. O(1) is here not precise enough.
*/


#ifndef _ASM_TYPEDEFS_H_
#define _ASM_TYPEDEFS_H_

/*! \brief Define the polynomial degree that ist used to represent 
    \brief "exponential". */
#define EXPONENTIAL 1000


/*! \brief Enum-values to specify how much output is desired
    \brief (verbosity-level).
*/

enum 
  { log_none,      /*!< Print no output (apart from error messages) */
    log_default,   /*!< Print the default information */
    log_verbose    /*!< Print verbose information */
  };
  

/*! \brief A list-representation of the dependencies of a nonterminal that
    \brief includes the asymptotical number of calls only (not the exact
    \brief polynomial number).

    We use a list because the number of dependent nonterminals can be 
    different for each nonterminal and there is no need to access one of this
    nonterminals directly.
*/

typedef struct asm_deps
  { struct asm_deps* next;  /*!< Pointer to the next entry in this list */
    int nt;                 /*!< The ID of the dependent nonterminal */
    int usages;             /*!< The asymptotical number of calls to this
                                 nonterminal */
  } *Asm_Deps;


/*! \brief An array-representation of the nonterminals that includes only that
    \brief information that is necessary to compute the asymptotical runtime.

    We use an array instead of a list because we often need direct access to
    one specific nonterminal - and the number of nonterminals does not change
    while the program runs.
*/

typedef struct asm_nts
  { int nt;         /*!< The ID of this nonterminal (we do not use a name) */
    int tab;        /*!< The state of the tabulation of this nonterminal:
                           0 ... tabulate it
                           1 ... do not tabulate it
                           2 ... optimize the state */
    int runtime;    /*!< The asymptotical runtime of this nonterminal on the
                         assumption that all of its parts are tabulated */
    int space;      /*!< Required space if this nontermal is tabulated:
                           O(1) ..... constant space
                           O(n) ..... linear space
                           O(n^2) ... square space */
    int usage;      /*!< An asymptotical value that describes how often this
                         nonterminal is used, according to an empty table
                         configuration. This information is not contained in
                         the "Nts"-representation given by the adp-compiler,
                         but it is to be computed by this program. The value
                         is -1 if the nonterminal is not in use. */
    Asm_Deps deps;  /*!< List of dependent nonterminals (i.e. all 
                         nonterminals that are called by this nonterminal) */
  } *Asm_Nts;


/*! \brief A list of nonterminals for various uses.
*/

typedef struct nt_list
  { struct nt_list* next;  /*!< Pointer to the next entry in this list */
    struct nt_list* prev;  /*!< Pointer to the previous entry in this list */
    int nt;                /*!< The ID of the nonterminal */
  } *Nt_List;


/*! \brief The Runstate-Type, which is used in "calc_asm_runtime" and
    \brief "asm_get_usages" to handle cycles in the dependency-graph
    \brief correctly.
    
    This is an array with one entry for each nonterminal. If the runtime-
    calculation (resp. usages-calculation) for a nonterminal is running then
    the runstate-field indicates the accumulated calls to dependent
    nonterminals which occur along a particular path in the dependence-graph.
    This value has to be evaluated if a nonterminal q is reached for the
    second time while the first function is still running (i.e. in case of
    cycles in the dependence-graph). The nonterminal for which this happens is
    detected as a recurrent nonterminal.
    
    In this case it depends on the runtime-value whether the runtime of q is
    affected by an additional factor of O(n) (if runstate equals 1) or if it
    is exponential (if runstate is greater than 1).
    
    The list stores all nonterminals for which a function is already running.
    This is used to avoid testing all nonterminals in the array for this
    condition. This allows a faster update of the accumulated usages.
*/

typedef struct run_type
  { int runstate;          /*!< The current running state of this 
                                nonterminal */
    struct nt_list* list;  /*!< The list of all currently running
                                nonterminals */
  } *Run_Type;
    

/*! \brief The runtime-record. It stores the asymptotical runtimes that
    \brief are achieved by several interesting table configurations.
*/
   
typedef struct runtime_info
  { int all;         /*!< Runtime if tabulating all nonterminals (ignores user
                          annotation) */
    int no;          /*!< Runtime if tabulating no nonterminals (ignores user
                          annotation) */
    int best;        /*!< Optimal runtime (ignores user annotation) */
    int all_user;    /*!< Runtime if tabulating all free nonterminals
                          (considers user annotation) */
    int no_user;     /*!< Runtime if tabulating no free nonterminals
                          (considers user annotation) */
    int best_user;   /*!< Optimal runtime with respect to the user
                          annotation */
  } Runtime_Info;
  

/*! \brief The nonterminal-number-record. It stores the number of the various
    \brief categories of nonterminals.
*/

typedef struct number_info
  { int total;   /*!< Total number of nonterminals */
    int used;    /*!< Number of nonterminals that are in use */
    int unused;  /*!< Number of nonterminals that are not in use */
    int user;    /*!< Number of nonterminals whose tabulation-state is defined
                      by the user annotation */
    int red;     /*!< Number of nonterminals whose tabulation-state is defined
                      by the preprocessing reductions */
    int lin;     /*!< Number of nonterminals that require less than square
                      space (in case of tabulation) */
    int opt;     /*!< Number of nonterminals whose tabulation-state is to be
                      defined by the brute-force-optimization (the
                      optimization-nonterminals) */
  } Number_Info;
  

/*! \brief The nonterminal-list-record. It stores lists of nonterminals of the
    \brief various categories.
*/
   
typedef struct nonterms_info
  { Nt_List unused;  /*!< List of all unused nonterminals */
    Nt_List user;    /*!< List of all nonterminals whose tabulation-state is 
                          defined by the user annotation */
    Nt_List red;     /*!< List of all nonterminals whose tabulation-state is 
                          defined by the preprocessing reductions */
    Nt_List lin;     /*!< List of all nonterminals that require less than
                          square space (in case of tabulation) */
    Nt_List opt;     /*!< List of all nonterminals whose tabulation-state is
                          to be defined by the brute-force-optimization
                          (the optimization-nonterminals) */
  } Nonterms_Info;
  

/*! \brief A List of table configurations. Each entry contains the tabulation
    \brief state for the optimization-nonterminals only. The tabulation state
    \brief of all other nonterminals always remains the same.
*/

typedef struct tabledesign_list
  { char* td;                       /*!< The tabulation state of the
                                         optimization-nonterminals:
                                           td[i] == 0 ... tabulated
                                           td[i] == 1 ... not tabulated */
    struct tabledesign_list* next;  /*!< Pointer to the next table
                                         configutation in the list */
  } *TableDesign_List;


/*! \brief The option-record. It stores the commandline-options. It is used by
    \brief "taboptimal.c", but not by "taboptimal_in.c" (there are no
    \brief options, but default values).
*/

typedef struct opt_rec
  { int print_dep_graph;   /*!< Print the internal representation of the
                                nonterminals and parser-dependencies and
                                finish the program ?  0 = no, 1 = yes */
    int user;              /*!< Consider user annotation ?  0 = no, 1 = yes */
    int red;               /*!< Apply preprocessing reductions ? 0 = no,
                                1 = yes */
    int nobest;            /*!< Use the optimal runtime computed in advance to
                                prune the search-space ?  0 = yes, 1 = no */
    int globalbest;        /*!< Calculate the best possible polynomial runtime
                                for table configurations with >= m tables ?
                                -1 = no, m = yes */ 
    int square;            /*!< Force square tables for each nonterminal ?
                                0 = no, 1 = yes */
    int lin;               /*!< Shall nonterminals that require less than
                                square space contribute to the cardinality of
                                the table configuration ?  0 = no, 1 = yes */
    int verbose;           /*!< Verbosity level:
                                  0 ... no output (apart from error-messages)
                                  1 ... default output
                                  2 ... verbose output */
    int bound;             /*!< Consider table configurations with <= m tables
                                only ?  -1 = no, m = yes */
    int exact;             /*!< Consider table configurations with exact m
                                tables only ?  -1 = no, m = yes */
    int TableDef;          /*!< Is a particular table configuration given
                                (option "-t") ?  0 = no, 1 = yes */ 
    Nt_List TableConf;     /*!< In case of use together with option "-t": All
                                nonterminals that are to be tabulated
                                In case of use together with option "-a": All
                                annotated nonterminals:
                                  ..->nt ==  q ... tabulate nonterminal q
                                  ..->nt == -q ... do not tabulate
                                                   nonterminal q */
    int time;              /*!< Seconds between two runtime-estimations */
    char* inputfilename;   /*!< The name of the inputfile (that contains the
                                serialized grammar) */
    char* resultfilename;  /*!< The name of the result-file */
    char* statefilename;   /*!< The name of the state-file */
  } Opt_Rec;
  
  
#endif
