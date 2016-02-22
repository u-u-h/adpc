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

/*! \file asm_runtime.c

    \brief Functions to deal with asymptotical runtimes and asymptotical
    \brief usages.

*/


#include <asm_runtime.h>


int poly2asm (ADPCPoly pol)
  
  { if (pol[0] == 1)
      if (pol[1] == 1)
        return 0;
      else
        return 1;
    else
      return pol[0];
  }


char* asm2string (int r)

  { char* s = malloc(15*sizeof(char));
    if (r == 0)
      return strcpy(s, "1");
    if (r == 1)
      return strcpy(s, "O(1)");
    if (r == 2)
      return strcpy(s, "O(n)");
    if (r == EXPONENTIAL)
      return strcpy(s, "O(2^n)");
    sprintf(s, "O(n^%d)", r-1);
    return s;
  }
  
  
inline int asm_times (int r1, int r2)

  { if (r1 == 0)
      return r2;
    if (r2 == 0)
      return r1;
    if ((r1 == EXPONENTIAL) || (r2 == EXPONENTIAL))
      return EXPONENTIAL;
    return r1+r2-1;
  }
  

inline int asm_plus (int r1, int r2)

  { if ((r1 == 0) && (r2 == 0))
      return 1;
    if (r1 > r2)
      return r1;
    return r2;
  }
  

int asm_best_runtime (Asm_Nts nonterms, int n, Run_Type run_state,
                      char* tab_runtime)

  { // Create a copy of some nonterminal-information to work with it
    Asm_Nts test_nonterms = malloc(n*sizeof(struct asm_nts));
    memcpy(test_nonterms, nonterms, n*sizeof(struct asm_nts));

    int i;
    // Get the usage of each nonterminal - with respect to the empty table
    // configuration
    for (i = 0; i < n; i++)
      if (nonterms[i].tab == 2)
        test_nonterms[i].tab = 1;
    asm_get_usages(test_nonterms,n,0);
    // Tabulate a nonterminal if and only if it is used more often than its
    // expenditure on tabulation, i.e. "usage > space" holds.
    for (i = 0; i < n; i++)
      if (nonterms[i].tab == 2)  // do not modify user annotation
        test_nonterms[i].tab
          = (nonterms[i].usage > nonterms[i].space) ? 0 : 1;
    
    // Calculate the runtime for this table configuration
    int best = calc_asm_runtime(test_nonterms,0,n,EXPONENTIAL,run_state,
                                tab_runtime);
    // Find out which nonterminals dominate the runtime and change their
    // tabulation state. Maybe the runtime gets better ...
    int new;
    for (i = 0; i < n; i++)
      if ((nonterms[i].usage > -1) && (nonterms[i].tab == 2))
        { new = calc_asm_runtime(test_nonterms,i,n,EXPONENTIAL,run_state,
                                 tab_runtime);
          // Does nonterminal i dominate the runtime ?
          if (new == best)
            { test_nonterms[i].tab = 1 - test_nonterms[i].tab;
              new = calc_asm_runtime(test_nonterms,0,n,EXPONENTIAL,run_state,
                                     tab_runtime);
              if (new < best)
                best = new;
              else
                test_nonterms[i].tab = 1 - test_nonterms[i].tab;
            }
        }
    
    // Get the usage of each nonterminal - with respect to the full table
    // configuration
    for (i = 0; i < n; i++)
      if (nonterms[i].tab == 2)  // do not modify user annotation
        test_nonterms[i].tab = 0;
    asm_get_usages(test_nonterms,n,0);
    // Do not tabulate a nonterminal if and only if it is used more often than
    // its expenditure on tabulation, i.e. "usage > space" holds.
    for (i = 0; i < n; i++)
      if (nonterms[i].tab == 2)
        test_nonterms[i].tab
           = test_nonterms[i].usage < test_nonterms[i].space ? 1 : 0;
    // Calculate the runtime for this table configuration
    int alt_best = calc_asm_runtime(test_nonterms,0,n,EXPONENTIAL,run_state,
                                    tab_runtime);
    // Find out which nonterminals dominate the runtime and change their
    // tabulation state. Maybe the runtime gets better ...
    for (i = 0; i < n; i++)
      if ((nonterms[i].usage > -1) && (nonterms[i].tab == 2))
        { new = calc_asm_runtime(test_nonterms,i,n,EXPONENTIAL,run_state,
                                 tab_runtime);
          // Does nonterminal i dominate the runtime ?
          if (new == alt_best)
            { test_nonterms[i].tab = 1 - test_nonterms[i].tab;
              new = calc_asm_runtime(test_nonterms,0,n,EXPONENTIAL,run_state,
                                     tab_runtime);
              if (new < alt_best)
                alt_best = new;
              else
                test_nonterms[i].tab = 1 - test_nonterms[i].tab;
            }
        }
    // Return the best runtime reached with the above heuristics
    if (alt_best < best)
      best = alt_best;
    
    free(test_nonterms);
    
    return best;
  }
   

/*! \brief Clear the running-state of a nonterminal.

    This is an auxiliary function for "asm_runtime" and "asm_get_usages_run".
    For a description of the running-state of nonterminal see
    "asm_typedefs.h". For its usage in detail see "asm_runtime" and
    "asm_get_usages_run".
    
    To clear the running-state of a nonterminal means to remove this
    nonterminal from the set of currently running nontermianls (to be precise,
    of course not the nonterminal itself is running, but the function
    "asm_runtime" resp. "asm_get_usages" for this nonterminal). This is
    necessary when the corresponding function finishes.
    
    The set of running nonterminals is implemented by a list, which is
    represented by two pointers: one for the first and one for the last
    element in the list.

    \param run_state Running-State-Array. It is to be modified.
    \param q The nonterminal whose running-state shall be cleared.
    \param RunLstBgn A Pointer to the first element in the list of running
                     nonterminals
    \param RunLstEnd A Pointer to the last element in the list of running
                     nonterminals
*/
  
static inline void asm_clear_runstate (Run_Type run_state, int q,
                                       Nt_List* RunLstBgn, Nt_List* RunLstEnd)
  
  { run_state[q].runstate = -1;  // -1 <-> not running
    Nt_List aux = run_state[q].list;
    run_state[q].list = NULL;
    if (aux->prev)
      aux->prev->next = aux->next;
    else
      *RunLstBgn = aux->next; 
    if (aux->next)
      aux->next->prev = aux->prev;
    else
      *RunLstEnd = aux->prev;
    free(aux);
  }


/*! \brief Calculate the runtime for a nonterminal with respect to a given
    \brief table configuration and create a list of all used tabulated
    \brief nonterminals. 
    
    Of course it is conceptually not necessary to compute a list of all used
    tabulated nonterminals, because one can simply use the tab-fields of
    the nonterminal-record to determine this information "online". However,
    therefore one have to test *all* nonterminals for this condition. There
    are some grammars where this would be significant slower, while it would
    be significant faster on others (because one avoids the expenditure on
    creating the list). According to some tests we decided to use the list.
    
    The function maintains a list of all nonterminals which are reached twice
    during the calculation. This are the recurrent nonterminals, i.e. they are
    involved in cycles in the dependence-graph. This list is used to handle
    such cycles correctly.
    
    This is a recursive function.
    
    \param nonterms Nonterminals. The tab-fields contain the table
                    configuation.
    \param n Number of nonterminals
    \param q The nonterminal whose runtime shall be calculated (usually the
             axiom).
    \param best The best possible (optimal) runtime. The calculation is
                aborted if the runtime of this table configuration exceeds the
                optimal runtime.
    \param run_state Running-State-Array. It has to be initialized as in
                     "taboptimal.c". This function modifies it to represent
                     state-information and restores the initialization when
                     it finishes. See "asm_typedefs.h".
    \param tab_runtime Tabulation-Array. It has to be initialized with zeros.
                       The entries of all tabulated nonterminals that are used
                       by the nonterminal "q" are to be set to 1. See 
                       "taboptimal.c".
    \param RunLstBgn Pointer to the first element in the list of running
                     nonterminals. It is used for recursive calls. Set it to
                     NULL as initial value - do not use a constant NULL, but
                     a variable (see "calc_asm_runtime").
    \param RunLstEnd Pointer to the last element in the list of running
                     nonterminals. It is used for recursive calls. Set it to
                     NULL as initial value - do not use a constant NULL, but
                     a variable (see "calc_asm_runtime"). 
    \param RecLstBgn Pointer to the first element in the list of recurrent
                     nonterminals (RESULT)
    \param RecLstEnd Pointer to the last element in the list of recurrent
                     nonterminals (RESULT)
    \param TabNtList The list of all tabulated nonterminals which are used by
                     the nonterminal "q" (RESULT)
    \param Result The asymptotical runtime of the nonterminal q (RESULT)
*/

static void asm_runtime (Asm_Nts nonterms, int n, int q, int best,
                         Run_Type run_state, char* tab_runtime,
                         Nt_List* RunLstBgn, Nt_List* RunLstEnd,
                         Nt_List* RecLstBgn, Nt_List* RecLstEnd,
                         Nt_List* TabNtList, int* Result)
 
  { // Is asm_runtime for nonterminal q already running ?
    if (run_state[q].list)
      { // Does the acculmulated usages of q equal 1 ? 
        if (run_state[q].runstate == 0)  // <-> 1
          { // Return q as a recurrent nonterminal. It will be handled later.
            *RecLstBgn = calloc(1,sizeof(struct nt_list));
            (*RecLstBgn)->nt = q;
            *RecLstEnd = *RecLstBgn;
            // Set the result and return. Conceptually the result should be
            // zero, but there is no representation for zero in the
            // asymptotical runtime domain that we use. We can set it to O(1);
            // this will not influence the over-all-result. (This does *not*
            // hold for the polynomial runtime, but for the asymptotical
            // runtime *only* !)
            *Result = 1;  // <-> O(1)
            return;
          }
        // The value of the accumulated usages is greater than 1
        else 
          { // The runtime is exponential in any case.
            *RecLstBgn = NULL;
            *RecLstEnd = NULL;
            *Result = EXPONENTIAL;
            return;
          }
      }
    
    // We encouter the nonterminal q for the first time in the current sub-
    // graph.
    else
      { // Initialize the accumulated usages of q
        run_state[q].runstate = 0;  // <-> 1 (asymptotical)
        // Add q to the list of the running nonterminals
        Nt_List aux = malloc(sizeof(struct nt_list));
        run_state[q].list = aux;
        aux->next = NULL;
        aux->nt = q;
        if (*RunLstBgn)
          { aux->prev = *RunLstEnd;
            (*RunLstEnd)->next = aux;
            *RunLstEnd = aux;
          }
        else
          { aux->prev = NULL;
            *RunLstBgn = aux;
            *RunLstEnd = aux;
          }
        // Initialization
        int Res = nonterms[q].runtime;
        Asm_Deps deps = nonterms[q].deps;
        *RecLstBgn = NULL;
        *RecLstEnd = NULL;
        // Handle all nonterminals p that are used by q
        Nt_List sub_RecLstBgn, sub_RecLstEnd;
        int sub_Result, p, k, run_nt;
        int* bak_prefactor;
        while (deps)
          { // Nonterminal q uses nonterminal p for k times
            p = deps->nt;
            k = deps->usages;
            // Handle tabulated nontermianls => table look-up
            if (nonterms[p].tab == 0)
              { // Add p to list of tabulated nonterminals
                if (tab_runtime[p] == 0)
                  { tab_runtime[p] = 1;
                    Nt_List auxlist = malloc(sizeof(struct nt_list));
                    auxlist->nt = p;
                    auxlist->next = *TabNtList;
                    *TabNtList = auxlist;
                  }
                // The runtime used for the k table look-ups is already
                // included in Res, because it is included in
                // nonterminal[q].runtime
              }
            // Handle nontabulated nonterminals => recursive call
            else
              { // Update the prefactors of all running nonterminals
                aux = *RunLstBgn;
                bak_prefactor = malloc(n*sizeof(int));
                while (aux)
                  { run_nt = aux->nt;
                    bak_prefactor[run_nt] = run_state[run_nt].runstate;
                    run_state[run_nt].runstate
                      = asm_times(run_state[run_nt].runstate,k);
                    aux = aux->next;
                  }
                // Calculate the runtime of p
                asm_runtime(nonterms,n,p,best,run_state,tab_runtime,RunLstBgn,
                            RunLstEnd,&sub_RecLstBgn,&sub_RecLstEnd,TabNtList,
                            &sub_Result);
                // Update the intermediate result
                Res = asm_plus(Res,asm_times(k,sub_Result));
                // Reset the prefactor of all running nonterminals
                aux = *RunLstBgn;
                while (aux)
                  { run_nt = aux->nt;
                    run_state[run_nt].runstate = bak_prefactor[run_nt];
                    aux = aux->next;
                  }
                free(bak_prefactor);
                // Update the recurrence set
                if (!(*RecLstBgn))
                  { *RecLstBgn = sub_RecLstBgn;
                    *RecLstEnd = sub_RecLstEnd;
                  }
                else if (sub_RecLstBgn)
                  { (*RecLstEnd)->next = sub_RecLstBgn;
                    *RecLstEnd = sub_RecLstEnd;
                  }
              }
            
            // Is the intermediate result worse than the optimal runtime ?
            if (Res > best)
              { // Abort. This table configuration is not optimal
                asm_clear_runstate(run_state, q, RunLstBgn, RunLstEnd);
                // Clear the list of recurrent nonterminals
                while (sub_RecLstBgn)
                  { aux = sub_RecLstBgn;
                    sub_RecLstBgn = sub_RecLstBgn->next;
                    free(aux);
                  }
                while (*RecLstBgn)
                  { aux = *RecLstBgn;
                    *RecLstBgn = (*RecLstBgn)->next;
                    free(aux);
                  }    
                *RecLstEnd = NULL;
                *Result = Res;
                return;
              }
            
            deps = deps->next;
          }
        
        // Do we have to solve a recurrence? That is the case if q is in the
        // list of recurrent nonterminals. 
        aux = *RecLstBgn;
        while (aux && (aux->nt != q))
          aux = aux->next;
        if (aux)
          { // Is q the only recurrent nonterminal ?
            if (*RecLstBgn == *RecLstEnd)
              { // Clear the list of recurrent nonterminals
                free(aux);
                *RecLstBgn = NULL;
                *RecLstEnd = NULL;
                asm_clear_runstate(run_state, q, RunLstBgn, RunLstEnd);
                // The runtime is effected by an additional factor of O(n)
                *Result = asm_times(Res,2);  // * O(n)
                return;
              }
            // There are at least two cycles
            else
              { // Clear the list of recurrent nonterminals
                while (*RecLstBgn)
                  { aux = *RecLstBgn;
                    *RecLstBgn = (*RecLstBgn)->next;
                    free(aux);
                  }
                *RecLstEnd = NULL;
                asm_clear_runstate(run_state, q, RunLstBgn, RunLstEnd);
                // The runtime is exponential in any case
                *Result = EXPONENTIAL;
                return;
              }
          }
        // q is no recurrent nonterminal at the moment.
        else
          { // We return the recurrent nonterminals to the upper recursion
            // level. There is an already running asm_runtime-function that
            // will handle them.
            asm_clear_runstate(run_state, q, RunLstBgn, RunLstEnd);
            *Result = Res;
          }
      }
  }


int calc_asm_runtime (Asm_Nts nonterms, int start, int n, int best,
                      Run_Type run_state, char* tab_runtime)

  { Nt_List RecLstBgn, RecLstEnd, aux, RunLstBgn = NULL, RunLstEnd = NULL,
            TabNtList = NULL;
    int Result, Res, nt;
    
    // Calculate the runtime of the nonterminal start first
    tab_runtime[start] = 1;
    asm_runtime(nonterms,n,start,best,run_state,tab_runtime,&RunLstBgn,
                &RunLstEnd,&RecLstBgn,&RecLstEnd,&TabNtList,&Result);
    // Consider the expenditure on tabulation if start is tabulated
    if (nonterms[start].tab == 0)
      Result = asm_times(Result, nonterms[start].space);
    
    // Calculate the runtime of used tabulated nonterminals
    aux = TabNtList;
    while (TabNtList && (Result <= best))
      { nt = TabNtList->nt;
        asm_runtime(nonterms,n,nt,best,run_state,tab_runtime,&RunLstBgn,
                    &RunLstEnd,&RecLstBgn,&RecLstEnd,&(TabNtList->next),&Res);
        Result = asm_plus(Result, asm_times(Res,nonterms[nt].space));
        TabNtList = TabNtList->next;
      }
    
    // Clear the list of tabulated nonterminals and the tabulation-array
    tab_runtime[start] = 0;
    while (aux)
      { TabNtList = aux;
        tab_runtime[TabNtList->nt] = 0;
        aux = aux->next;
        free(TabNtList);
      }
    
    return Result;
  }
  

int calc_asm_runtime_notab (Asm_Nts nonterms, int start, int n, int best,
                            Run_Type run_state, char* tab_runtime)

  { Nt_List RecLstBgn, RecLstEnd, aux, RunLstBgn = NULL, RunLstEnd = NULL,
            TabNtList = NULL;
    int Result;
    
    // Calculate the runtime of the nonterminal start only. (And do not
    // consider its expenditure on tabulation)
    tab_runtime[start] = 1;
    asm_runtime(nonterms,n,start,best,run_state,tab_runtime,&RunLstBgn,
                &RunLstEnd,&RecLstBgn,&RecLstEnd,&TabNtList,&Result);
    
    // Clear the list of tabulated nonterminals and the tabulation-array
    tab_runtime[start] = 0;
    aux = TabNtList;
    while (aux)
      { TabNtList = aux;
        tab_runtime[TabNtList->nt] = 0;
        aux = aux->next;
        free(TabNtList);
      }
    
    return Result;
  }
  

/*! \brief Propagate an additional usage-factor to all reachable nonterminals
    \brief in the dependece-graph. This is an auxiliary function which is used
    \brief by "asm_get_usages_run".

    If a recurrent nonterminal q is handled, there is always an additional
    factor of O(n) or O(2^n). This factor increases the number of calls to
    each dependent nonterminal (i.e. its usage). But the recurrence of q is
    detected only after "asm_get_usages_run" is returned from recursion and
    all dependent nonterminals have been handled. So we have to consider each
    dependent nonterminal one more time and update its usage.
    
    Because there could be more than one cycle, we us an array "rec_factor"
    that accumulates the additional factors for all cycles a nonterminal
    appears in.
    
    The dependent nonterminals are all nonterminals that can be reached from
    q.
    
    This is a recursive function.
    
    \param nonterms Nonterminals
    \param n Number of Nonterminals
    \param prop_state Propagation-state of the nonterminals. A nonterminal q
                      has already been visited if and only if "prop_state[q]"
                      equals 1. "prop_state" has to be initialized with zeros.
                      This initialization is restored when the function
                      finishes.
    \param rec_factor Recurrence-factor-array. "rec_factor[q]" contains the
                      additional recurrence-factor for nonterminal q. See
                      "asm_get_usages_run".
    \param q The nonterminal whose usage is to be updated (including the usage
             of all its dependent nonterminals)
    \param m The "regular" usage of q (without any usage that results from
             recurrence factors). See "asm_get_usages_run".
*/

static void asm_prop_result (Asm_Nts nonterms, int n, char* prop_state,
                             int* rec_factor, int q, int m)

  { if (prop_state[q] == 0)
      { prop_state[q] = 1;
        m = asm_times(m,rec_factor[q]);
        if (nonterms[q].usage == -1)
          nonterms[q].usage = m;
        else
          // Maybe we add the same usage for several times. But this does not
          // matter because we consider the asymptotical usages only.
          nonterms[q].usage = asm_plus(nonterms[q].usage,m);
        Asm_Deps deps = nonterms[q].deps;
        while (deps)
          { if (nonterms[deps->nt].tab != 0)
              asm_prop_result(nonterms,n,prop_state,rec_factor,deps->nt,
                              asm_times(m,deps->usages));
            deps = deps->next;
          }
        // Clear the propagation-state of q for the next call to
        // asm_prop_result
        prop_state[q] = 0;
      }
  }


/*! \brief Get the usage of a nonterminal. This is an auxiliary-function that
    \brief is used by "asm_get_usages".

    The structure of this function is very similiar to "asm_runtime". Please
    see the description there.
        
    This is a recursive function.
    
    \param nonterms Nonterminals. The tab-fields contain the table
                    configuation.
    \param n Number of nonterminals
    \param q The nonterminal whose usage shall be calculated (usually the
             axiom).
    \param m The "regular" usage of q without any usage that results from
             recurrence factors. Use 0 as initial value.
    \param run_state Running-State-Array. It has to be initialized as in
                     "taboptimal.c". This function modifies it to represent
                     state-information and restores the initialization when
                     it finishes. See "asm_typedefs.h".
    \param rec_factor Recurrence-factor-array. "rec_factor[q]" contains the
                      additional recurrence-factor for nonterminal q. This
                      array is maintained by this function. Initialize with
                      zeros.
    \param tab_state Tabulation-Array. It has to be initialized with zeros.
                     The entries of all tabulated nonterminals that are used
                     by the nonterminal "q" are to be set to 1. See
                     "taboptimal.c".
    \param RunLstBgn Pointer to the first element in the list of running
                     nonterminals. It is used for recursive calls. Set it to
                     NULL as initial value - do not use a constant NULL, but
                     a variable (see "asm_get_usages").
    \param RunLstEnd Pointer to the last element in the list of running
                     nonterminals. It is used for recursive calls. Set it to
                     NULL as initial value - do not use a constant NULL, but
                     a variable (see "asm_get_usages"). 
    \param RecLstBgn Pointer to the first element in the list of recurrent
                     nonterminals (RESULT)
    \param RecLstEnd Pointer to the last element in the list of recurrent
                     nonterminals (RESULT)
    \param TabNtList The list of all tabulated nonterminals which are used by
                     the nonterminal "q" (RESULT)
*/

static void asm_get_usages_run (Asm_Nts nonterms, int n, int q, int m,
                                Run_Type run_state, int* rec_factor,
                                char* tab_state, char* prop_state,
                                Nt_List* RunLstBgn, Nt_List* RunLstEnd,
                                Nt_List* RecLstBgn, Nt_List* RecLstEnd,
                                Nt_List* TabNtList)
 
  { // Is asm_runtime for q already running ?
    if (run_state[q].list)
      { // Return q as a recurrent nonterminal
        *RecLstBgn = calloc(1,sizeof(struct nt_list));
        (*RecLstBgn)->nt = q;
        *RecLstEnd = *RecLstBgn;
        // Does the accumulated usages equal 1 ?
        if (run_state[q].runstate == 0)  // <-> 1
          rec_factor[q] = 2;  // <-> O(n)
        // The value of the accumulated usages is greater than 1
        else
          rec_factor[q] = EXPONENTIAL;
        return;
      }
    
    // The nonterminal q is encountered for the first time in this sub-graph.
    else 
      { Nt_List sub_RecLstBgn, sub_RecLstEnd, aux;
        run_state[q].runstate = 0;
        // Add q to the list of the running nonterminals
        aux = malloc(sizeof(struct nt_list));
        run_state[q].list = aux;
        aux->next = NULL;
        aux->nt = q;
        if (*RunLstBgn)
          { aux->prev = *RunLstEnd;
            (*RunLstEnd)->next = aux;
            *RunLstEnd = aux;
          }
        else
          { aux->prev = NULL;
            *RunLstBgn = aux;
            *RunLstEnd = aux;
          }
        // Initialization
        Asm_Deps deps = nonterms[q].deps;
        *RecLstBgn = NULL;
        *RecLstEnd = NULL;
        // Handle all nonterminals p that are used by q
        int p, k, run_nt;
        int* bak_prefactor;
        while (deps)
          { // Nonterminal q uses nonterminal p for k times
            p = deps->nt;
            k = deps->usages;
            // Update the usage for nonterminal p
            if (nonterms[p].usage == -1)
              nonterms[p].usage = asm_times(m,k);
            else
              nonterms[p].usage = asm_plus(asm_times(m,k),nonterms[p].usage);
            // Is nonterminal p tabulated ?
            if (nonterms[p].tab == 0)
              { // Add p to the list of tabulated nonterminals
                if (tab_state[p] == 0)
                  { tab_state[p] = 1;
                    Nt_List auxlist = malloc(sizeof(struct nt_list));
                    auxlist->nt = p;
                    auxlist->next = *TabNtList;
                    *TabNtList = auxlist;
                  }
              }
            // p is not tabulated
            else
              { aux = *RunLstBgn;
                bak_prefactor = malloc(n*sizeof(int));
                while (aux)
                  { run_nt = aux->nt;
                    bak_prefactor[run_nt] = run_state[run_nt].runstate;
                    run_state[run_nt].runstate
                      = asm_times(run_state[run_nt].runstate,k);
                    aux = aux->next;
                  }
                // Update the usages of all nonterminals that are used by p
                asm_get_usages_run(nonterms,n,p,asm_times(m,k),run_state,
                                   rec_factor,tab_state,prop_state,RunLstBgn,
                                   RunLstEnd,&sub_RecLstBgn,&sub_RecLstEnd,
                                   TabNtList);
                // Reset the prefactor of all running nonterminals
                aux = *RunLstBgn;
                while (aux)
                  { run_nt = aux->nt;
                    run_state[run_nt].runstate = bak_prefactor[run_nt];
                    aux = aux->next;
                  }
                free(bak_prefactor);
                // Update the list of recurrent nonterminals
                if (!(*RecLstBgn))
                  { *RecLstBgn = sub_RecLstBgn;
                    *RecLstEnd = sub_RecLstEnd;
                  }
                else if (sub_RecLstBgn)
                  { (*RecLstEnd)->next = sub_RecLstBgn;
                    *RecLstEnd = sub_RecLstEnd;
                  }
              }
            
            deps = deps->next;
          }
        
        // Do we have to solve a recurrence? That is the case if q is in the
        // list of recurrent nonterminals. 
        aux = *RecLstBgn;
        while (aux && (aux->nt != q))
          aux = aux->next;
        if (aux)
          { // Is q the only recurrent nonterminal ?
            if (*RecLstBgn == *RecLstEnd)
              { // Clear the list of recurrent nonterminals
                free(aux);
                *RecLstBgn = NULL;
                *RecLstEnd = NULL;
                asm_clear_runstate(run_state, q, RunLstBgn, RunLstEnd);
                // The recurrence-factor for q has already been set
                asm_prop_result(nonterms, n, prop_state, rec_factor, q, m);
                return;
              }
            // There are at least two cycles
            else
              { // Clear the list of recurrent nonterminals
                while (*RecLstBgn)
                  { aux = *RecLstBgn;
                    *RecLstBgn = (*RecLstBgn)->next;
                    free(aux);
                  }
                *RecLstEnd = NULL;
                asm_clear_runstate(run_state, q, RunLstBgn, RunLstEnd);
                // The usage of q and all dependent nonterminals are
                // exponential in any case
                rec_factor[q] = EXPONENTIAL;
                asm_prop_result(nonterms, n, prop_state, rec_factor, q, m);
                return;
              }
          }
        
        // We return the recurrent nonterminals to the upper recursion level.
        // There is an already running asm_runtime-function that will handle
        // them.
        asm_clear_runstate(run_state, q, RunLstBgn, RunLstEnd);
      }
  }
                   

void asm_get_usages (Asm_Nts nonterms, int n, int start)

  { Nt_List RecLstBgn, RecLstEnd, aux, RunLstBgn = NULL, RunLstEnd = NULL,
            TabNtList = NULL;
    // Initialize the arrays that are required for asm_get_usages_run
    Run_Type run_state = malloc(n*sizeof(struct run_type));
    int i, m, nt;
    for (i = 0; i < n; i++) 
      { run_state[i].runstate = -1;
        run_state[i].list = NULL;
        nonterms[i].usage = -1;
      }
    char* tab_state = calloc(n,sizeof(char));
    char* prop_state = calloc(n,sizeof(char));
    int* rec_factor = calloc(n,sizeof(int));
    
    // Get the usage of the nonterminal start first
    tab_state[start] = 1;
    nonterms[start].usage = 0;  // <-> 1
    // Consider the expenditure on tabulation if start is tabulated
    if (nonterms[start].tab == 0)
      m = nonterms[start].space;
    else 
      m = 0;  // <-> 1
    asm_get_usages_run(nonterms,n,start,m,run_state,rec_factor,tab_state,
                       prop_state,&RunLstBgn,&RunLstEnd,&RecLstBgn,&RecLstEnd,
                       &TabNtList);
    
    // Calculate the usage of used tabulated nonterminals
    aux = TabNtList;
    while (TabNtList)
      { // rec_factor[*] is not guaranteed to be empty when asm_get_usages_run
        // finishes - so we explicitly clear it
        for (i = 0; i < n; i++)
          rec_factor[i] = 0;
        nt = TabNtList->nt;
        m = nonterms[nt].space;
        asm_get_usages_run(nonterms,n,nt,m,run_state,rec_factor,tab_state,
                           prop_state,&RunLstBgn,&RunLstEnd,&RecLstBgn,
                           &RecLstEnd,&(TabNtList->next));
        TabNtList = TabNtList->next;
      }
  
    // Clear the list of tabulated nonterminals and the tabulation-array
    tab_state[start] = 0;
    while (aux)
      { TabNtList = aux;
        tab_state[TabNtList->nt] = 0;
        aux = aux->next;
        free(TabNtList);
      }
    free(run_state);
    free(rec_factor);
    free(tab_state); 
    free(prop_state);
  }
  

void asm_apply_reductions (Asm_Nts nonterms, int n, Run_Type run_state,
                           char* tab_state, int* opt_nontermstate, int* n_red,
                           Nt_List* list, Opt_Rec* options)
  
  { // Create two copies of the nonterminal-fields to modify the table
    // configuration: test_nonterms1: empty table configuration;
    // test_nonterms2: full table configuration
    Asm_Nts test_nonterms1 = malloc(n*sizeof(struct asm_nts));
    Asm_Nts test_nonterms2 = malloc(n*sizeof(struct asm_nts));
    memcpy(test_nonterms1, nonterms, n*sizeof(struct asm_nts));
    memcpy(test_nonterms2, nonterms, n*sizeof(struct asm_nts));
    int i, red, rt, a, b;
    for (i = 0; i < n; i++)
      { test_nonterms1[i].tab = 1;
        test_nonterms2[i].tab = 0;
      }
    Nt_List aux;
    *list = NULL;
    *n_red = 0;
    // Use reverse order to get the list in rising order
    for (i = n-1; i >= 0; i--)
      if (nonterms[i].usage == -1)  // do not consider unused nonterminals
        opt_nontermstate[i] = 1;
      else if (nonterms[i].tab == 2)  // only if no user annotation
        { red = 0;
          // Apply reduction 1: Nonterminals with constant runtime need no
          // tabulation. This does not hold if the option "-g" or "-e" is set.
          // If the option "-l" is not set (default), it does not hold for
          // nonterminals that require less than square space, because they do
          // not contribute to the cardinality of the table configuration and
          // tabulating them could improve the constant factors. But even
          // there is an exception: If the runtime of a nonterminal equals 1
          // then there is no way how tabulation could improve the constant
          // factors, thus this nonterminal needs no tabulation.
          rt = calc_asm_runtime_notab(test_nonterms1,i,n,2,run_state,
                                      tab_state);
          if ((rt <= 1) && 
              (((!options || options->globalbest == -1) &&
                (!options || options->exact == -1) &&
                ((nonterms[i].space == 3) || (options && options->lin))) ||
               (rt == 0)))
             { red = 1;
               nonterms[i].tab = 1;
             }
          // Apply reduction 2: Nonterminals that are used for one time only
          // need no tabulation. Also nonterminals with "usage <= space" need
          // no tabulation, but only if they require square space or if the
          // option "-l" is set. The last statement does not hold if the
          // option "-g" or "-e" is set, because tabulating such a
          // nonterminal could improve the constant factors.
          else if ((nonterms[i].usage == 0) ||
                   ((nonterms[i].usage <= nonterms[i].space) &&
                    ((nonterms[i].space == 3) || (options && options->lin)) &&
                    (!options || (options->globalbest == -1)) &&
                    (!options || (options->exact == -1))))
            { red = 1;
              nonterms[i].tab = 1;
            }
          // Apply reduction 3: A nonterminal that increases the runtime
          // asymptotically, if it is the only one which is not tabulated, has
          // to be tabulated. This nonterminal is responsible for the increase
          // of the runtime. In some very special cases this may fail and thus
          // a nonterminal will be tabulated which should not be tabulated.
          // But these special cases seem not to be of any practical use, so
          // we perform a simple heuristic to avoid some of them and just
          // ignore all other ones.
          else
            { rt = calc_asm_runtime(test_nonterms2,0,n,EXPONENTIAL,run_state,
                                    tab_state);
              test_nonterms2[i].tab = 1;
              if (calc_asm_runtime(test_nonterms2,0,n,EXPONENTIAL,run_state,
                                   tab_state) > rt)
                { red = 1;
                  // Try to exclude some other nonterminals from the table
                  // configuration => We exclude all Pairs and Tripel to see whether
                  // there is a special case. We consider the user annotation.
                  for (a = i-1; a >= 1; a--)
                    if (nonterms[a].tab != 0)  // user annotation
                      { test_nonterms2[a].tab = 1;
                        if (calc_asm_runtime(test_nonterms2,0,n,EXPONENTIAL,
                            run_state,tab_state) <= rt)
                          red = 0;
                        for (b = a-1; b >= 0; b--)
                          if (nonterms[b].tab != 0)  // user annotation
                            { test_nonterms2[b].tab = 1;
                              if (calc_asm_runtime(test_nonterms2,0,n,
                                                   EXPONENTIAL,run_state,
                                                   tab_state) <= rt)
                                red = 0;
                              test_nonterms2[b].tab = 0;
                            }
                        test_nonterms2[a].tab = 0;
                      }
                  // if no excluded pair or tripel compensated the runtime
                  // increment
                  if (red == 1)
                    nonterms[i].tab = 0;
                }
              test_nonterms2[i].tab = 0;
            }
          // Did any reduction succed ?
          if (red == 1)
            { (*n_red)++;  
              opt_nontermstate[i] = 1;
              aux = malloc(sizeof(struct nt_list));
              aux->nt = i;
              aux->next = *list;
              if (*list)
                (*list)->prev = aux;
              *list = aux;
            }
        }
    if (*list)
      (*list)->prev = NULL;
    free(test_nonterms1);
    free(test_nonterms2);
  }
