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

/*! \file functions.c

    \brief Auxiliary functions that are used by both "taboptimal.c" and
    \brief "taboptimal_in.c".

*/

 
#include <functions.h>

#include <assert.h>


void update_table_configuration (Asm_Nts nonterms, int n_opt, char* td,
                                 int* opt_nonterms)

  { int i;
    for (i = 0; i < n_opt; i++)
      nonterms[opt_nonterms[i]].tab = td[i];
  }


void convert_table_configuration (Asm_Nts nonterms, int n,
                                  struct graph* graph)

  { int i;
    struct nt_vertex *v;
    //printf("Convert table configuration: ");
    for (i = 0; i < n; i++)
      { v = (struct nt_vertex*) graph_vertex(graph, i+1);
        // Do not tabulate unused nonterminals
        //v->tabulated = (nonterms[i].usage > -1) ? 1 - nonterms[i].tab : 0;
        
        assert((nonterms[i].tab >= 0) && (nonterms[i].tab <= 2));
        if (nonterms[i].usage == -1)
          v->tabulated = -1;
        else {
          switch (nonterms[i].tab) {
            case 0 : v->tabulated = 1;
                     break;
            case 1 : v->tabulated = -1;
                     break;
            case 2 : v->tabulated = 0;
                     break;
          }
        }
        //printf("%d ", v->tabulated);

        // Attention:
        //   tabulation    <-> v->tabulated == 1 <-> nonterms[i].tab == 0
        //   no tabulation <-> v->tabulated == 0 <-> nonterms[i].tab == 1
      }
    //printf("\n");
  }

  
void get_best_polynomial_runtime (Asm_Nts nonterms, Number_Info number,
                                  TableDesign_List Tables, 
                                  struct graph* graph, int* opt_nonterms,
                                  Poly* best, unsigned int* best_count)

  { Poly runtime;
    *best = poly_exp();  // exponential
    *best_count = 0;
    TableDesign_List CurTable = Tables;
    while (CurTable)
      { update_table_configuration(nonterms,number.opt,CurTable->td,
                                   opt_nonterms);
        convert_table_configuration(nonterms,number.total,graph);
        runtime = compute_runtime(graph);
        // Is runtime better than *best ?
        if (poly_compare(*best,runtime) == 1) 
          { free(*best);
            *best = runtime;
            *best_count = 1;
          }
        // Are runtime and *best equal ?
        else if (poly_compare(*best,runtime) == 0)
          { (*best_count)++;
            free(runtime);
          }
        // Is runtime worse then *best ?
        else
          free(runtime);
        CurTable = CurTable->next;
      }
  }


void list_unused_nonterms (Asm_Nts nonterms, int n, int* opt_nontermstate,
                           int* n_unused, Nt_List* list)

  { *n_unused = 0;
    *list = NULL;
    Nt_List aux;
    int i;
    // Use reverse order to get the list in rising order
    for (i = n-1; i >= 0; i--)
      if (nonterms[i].usage == -1)
        { opt_nontermstate[i] = 1;
          aux = malloc(sizeof(struct nt_list));
          aux->nt = i;
          aux->next = *list;
          if (*list)
            (*list)->prev = aux;
          *list = aux;
          (*n_unused)++;
        }
    if (*list)
      (*list)->prev = NULL;
  }


void list_lin_nonterms (Asm_Nts nonterms, int n, int* n_lin, Nt_List* list)

  { *n_lin = 0;
    *list = NULL;
    Nt_List aux;
    int i;
    // Use reverse order to get the list in rising order
    for (i = n-1; i >= 0; i--)
      if (nonterms[i].space < 3)  // < O(n^2)
        { aux = malloc(sizeof(struct nt_list));
          aux->nt = i;
          aux->next = *list;
          if (*list)
            (*list)->prev = aux;
          *list = aux;
          (*n_lin)++;
      }
    if (*list)
      (*list)->prev = NULL;
  }


void set_user_configuration(Asm_Nts nonterms, int n, int* opt_nontermstate,
                            int* n_user, Nt_List* list)
  
  { Nt_List aux;
    *list = NULL;
    *n_user = 0;
    int i;
    // Use reverse order to get the list in rising order
    for (i = n-1; i >= 0; i--)
      // Is nonterminal i unused ?
      if (nonterms[i].usage == -1)  
        opt_nontermstate[i] = 1;  // Do not optimize unused nonterminals
      // Is nonterminal i annotated by the user ?
      else if (nonterms[i].tab != 2)
        { (*n_user)++;  
          opt_nontermstate[i] = 1;  // Do not optimize annotated nonterminals
          aux = malloc(sizeof(struct nt_list));
          aux->nt = i;
          aux->next = *list;
          if (*list)
            (*list)->prev = aux;
          *list = aux;
        }
    if (*list)
      (*list)->prev = NULL;
  }


void set_opt_nonterms (int* opt_nontermstate, int n, int* opt_nonterms,
                       int* opt_n, Nt_List* list)

  { Nt_List aux;
    *list = NULL;
    int i;
    *opt_n = 0;
    // Use reverse order to get the list in rising order
    for (i = n-1; i >= 0; i--)
      // Does nonterminal i remain for optimization ?
      if (opt_nontermstate[i] == 0)
        { opt_nonterms[*opt_n] = i;
          (*opt_n)++;
          aux = malloc(sizeof(struct nt_list));
          aux->nt = i;
          aux->next = *list;
          if (*list)
            (*list)->prev = aux;
          *list = aux;
        }
    if (*list)
      (*list)->prev = NULL;
  }
