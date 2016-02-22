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

/*! \file tabulate.c
    
    \brief Implementation of the interface-functions Haskell <-> C.
    
*/


#include <tabulate.h>


void printPoly (FILE* file, ADPCPoly p) {
  int i;
  char plus = 0;
  for (i=1; i<=p[0]; i++) { 
    if (p[i] != 0) {
      fprintf(file, "%d", p[i]);
      if (i==2)
        fprintf(file, "n");
      if (i>2)
        fprintf(file, "n^%d", i-1);
      plus = 1;
    }
    if (i<p[0] && plus)
      fprintf(file, "+");
  }
}


void printDeps (FILE* file, Deps deps) {
  Deps itr;
  itr = deps;
  while (itr) {
    fprintf(file, "Nt: %d, usages: ", itr->nt);
    printPoly(file, itr->usages);
    fprintf(file, "\n");
    itr = itr->next;
  } 
}


void printNts (FILE* file, Nts nts) {
  Nts itr;
  itr = nts;
  while (itr) {
    fprintf(file, "----------------------------------------\n");
    fprintf(file, "Nt: %d, user configuration: %d, runtime :", itr->nt,
                  itr->tab);
    printPoly(file, itr->runtime);
    fprintf(file, ", tabulation: ");
    printPoly(file, itr->tabulationType);
    fprintf(file, "\nDependencies:\n");
    printDeps (file, itr->deps);
    itr = itr->next;
  }
}


/*! \brief Read a polynomial from the current position in the array and sets
    \brief this position after the polynomial - Auxiliary-function of
    \brief "deserializeGraph".
    \param arr The serialized array
    \param ap Current postion in "arr" (will be updated)
    \return The polynomial
*/

static ADPCPoly readPoly (int *arr, int *ap) {
  int i, count;
  ADPCPoly poly;

  count = arr[*ap];
  poly = calloc(count+1, sizeof(int));  
  for (i=0; i<=count; i++)
    poly[i] = arr[(*ap)++];

  return poly;
}


Nts deserializeGraph (int *arr, int *size) {
  Nts nt, nextNt = NULL, result = NULL;
  Deps dep, nextDep = NULL;
  int ap, i, j, count, ndeps;
  ap = 0;

  count = arr[ap++];
  for (i=1; i<=count; i++) {

    nt = malloc(sizeof(struct nts));
    if (i==1)
      result = nt;
    else
      nextNt->next = nt;
    nt->next    = NULL;
    nextNt      = nt;

    nt->nt      = arr[ap++];
    nt->tab     = arr[ap++];
    nt->tabulationType = readPoly(arr, &ap);
    nt->runtime        = readPoly(arr, &ap);
    ndeps = arr[ap++];
    if (!ndeps) {
      nt->deps = NULL;
      continue;
    }
    for (j=1; j<=ndeps; j++){

      dep = malloc(sizeof(struct deps));
      if (j==1)
        nt->deps = dep;
      else
        nextDep->next = dep;
      dep->next = NULL;
      nextDep = dep;

      dep->nt = arr[ap++];
      dep->usages = readPoly(arr, &ap);
    }
  }
  *size = ap;
  return result;
}


void serialize_array (FILE* file, int *arr, int size) {
  int i;
  for (i=0; i<size; i++) {
    fprintf(file, "%d;", arr[i]);
  }
  fprintf(file, "\n");
}


void read_array (FILE* file, int** arr, int* size) {
  int default_array_size = 1000;
  int i = 0, k = 1, error = 0, x;
  char c;
  *arr = malloc(default_array_size*sizeof(int));
  fscanf(file, "\n");
  while ((!feof(file)) && (error == 0))
    { fscanf(file, "%d", &x);
      fscanf(file, "%c", &c);
      if (c != ';') error = 1;
      fscanf(file, "\n");
      if (error == 0)
        { (*arr)[i] = x;
          if ((i++) % default_array_size == 0)
            *arr = realloc(*arr, (++k)*default_array_size*sizeof(int));
        }
    }
  if (error)
    { free(*arr);
      *arr = NULL;
      *size = 0;
    }
  else
    { *arr = realloc(*arr, i*sizeof(int));
      *size = i;
    }
}


/*! \brief Free the memory used to store deps - Auxiliary-function of
    \brief "nts_delete".
    \param deps Dependency-list to delete
*/

static void deps_delete (Deps deps)
{
  Deps itr, t;
  itr = deps;
  while (itr) {
    free(itr->usages);
    t = itr;
    itr = itr->next;
    free(t);
  } 
}


void nts_delete (Nts nts) {
  Nts itr, t;
  itr = nts;
  while (itr) {
    free(itr->runtime);
    free(itr->tabulationType);
    deps_delete(itr->deps);
    t = itr;
    itr = itr->next;
    free(t);
  }
}
