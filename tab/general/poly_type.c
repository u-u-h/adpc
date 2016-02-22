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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "poly_type.h"

#include <assert.h>

/* Ein Polynom der Form 
     x_0 * n^0 + x_1 * n^1 + ... + x_l * n^l
   wird wie folgt gespeichert:
     [l, x_0, x_1, .. ,x_l]
*/

Poly poly_copy(Poly p, int s) {
  int i;
  int x;
  x = p[0] > s ? p[0] : s;
  Poly r = calloc(x + 2, sizeof(Factor));
  for (i=0; i < p[0]+1; i++)
    r[1+i] = p[1+i];
  r[0] = p[0];
  return r;
}

void poly_add(Poly p1, Poly p2) {
  int i;
  if (p1[0] < p2[0])
    p1[0] = p2[0];
  for (i = 0; i < p2[0]+1; i++)
    p1[1+i] += p2[1+i];
}

void poly_sub(Poly p1, Poly p2) {
  int i;
  int max_l = p1[0] < p2[0] ? p2[0] : p1[0];
  int real_l = max_l;
  for (i = 0; i < max_l+1; i++) {
    p1[1+i] -= p2[1+i];
    // assert(p1[1+i] > -1); // uint ...
  }

  for (i = max_l; i>0; i--)
    if (p1[1+i] == 0)
      real_l--;
    else
      break;

  p1[0] = real_l;

}

Poly poly_time(int l, Factor x, Poly p2, Poly result) {
  int i;
  if (l == 0) {
    for (i=0; i < p2[0]+1; i++)
      result[1+i] += p2[1+i] * x;
  } else {
    for (i=0; i < p2[0]+1; i++)
      result[1+l+i] += p2[1+i] * x;
  }
  return result;
}

Poly poly_times(Poly p1, Poly p2, int s) {
  int i;
  int x;
  Poly result;

  x = s > p1[0] + p2[0] ? s : p1[0] + p2[0];
  result = calloc(x + 2, sizeof(Factor));
  if (p1[0] + p2[0] >  s) {
    result[0] = s;
    result[1+s] = 1;
    return result;
  }
  result[0] = p1[0] + p2[0];
  for (i = 0; i < p1[0]+1; i++) {
    if (p1[i+1] == 0)
      continue;
    poly_time(i, p1[i+1], p2, result);
  }
  return result;
}
Poly poly_times_free(Poly p1, Poly p2, int s) {
  Poly r;
  r = poly_times(p1, p2, s);
  free(p1);
  return r;
}

/* returns j, 0<=i<=l, min(i) with x_j > 0 */
int poly_min_degree(Poly p) {
  int i;
  int l;
  l = p[0];
  for (i=0; i<l+1; i++)
    if (p[1+i] > 0)
      return i;
  /* take care of empty input */
  return 0;
  }


void poly_div(Poly p1, Poly p2, int s)
{
  int i, j, k, l1, l2, l2s;
  Factor *a1, *a2;
  l1 = p1[0];
  l2 = p2[0];
  a1 = p1 + 1;
  a2 = p2 + 1;
  assert(p1[0] >= p2[0]);
  p1[0] = l1-l2;

  l2s = poly_min_degree(p2);

  for (i=0; i<l1+1; i++) {
    if (a1[i] == 0)
      continue;
    assert(a1[i] % a2[l2s] == 0);
    assert(i-l2s > -1);
    a1[i-l2s] = a1[i] / a2[l2s];
    for (j=l2s+1; j<l2+1; j++)
      for (k=i+1; k<l1+1; k++) {
        assert(k+i-l2s > -1);
        a1[k+i-l2s] -= a1[i-l2s] * a2[k];
      }
  }
}

void poly_print(FILE *f, Poly p)
{
  int i;
  for (i=0; i<p[0]+1; i++) {
    if (p[1+i] != 0) {
      if (i==0)
        fprintf(f, "%" PRIfactor, p[1+i]);
      else
        if (p[1+i]>1)
          fprintf(f, "%" PRIfactor, p[1+i]);
      if (i==1)
        fprintf(f, "n");
      if (i>1)
        fprintf(f, "n^%d", i);
      if (i<p[0])
        fprintf(f, " + ");
    }
  }
}

void poly_snprint(char *s, size_t size, Poly p)
{
  int i;

  size_t j,k;
  j = size;

  for (i=0; i<p[0]+1; i++) {
    if (p[1+i] != 0) {
      if (i==0) {
        k = snprintf(s, j, "%" PRIfactor, p[1+i]);
        s += k;
        j -= k;
      }
      else
        if (p[1+i]>1) {
          k = snprintf(s, j, "%" PRIfactor, p[1+i]);
          s +=k;
          j -= k;
        }
      if (i==1) {
        k = snprintf(s, j, "n");
        s += k;
        j -= k;
      }
      if (i>1) {
        k = snprintf(s, j, "n^%d", i);
        k = snprintf(s, j, "n");
        s += k;
        j -= k;
      }
      if (i<p[0]) {
        k = snprintf(s, j, " + ");
        k = snprintf(s, j, "n");
        s += k;
        j -= k;
      }
      if (j<1)
        return;
    }
  }
}

Poly poly_n(int i, int s) {
  Poly r;
  r = calloc(s+2, sizeof(Factor));
  r[0] = i;
  r[1+i] = 1;
  return r;
}

Poly poly_exp() {
  return poly_n(20, 20);
}

int poly_exponential(Poly p) {
  return p[0] > 19;
}

Poly poly_ill()
{
  return poly_n(19, 20);
}

int poly_illegal(Poly p)
{
  return p[0] == 19;
}

Poly poly_int(Factor i, int s) {
  Poly r;
  r = calloc(s+2, sizeof(Factor));
  r[0] = 0;
  r[1] = i;
  return r;
}

int poly_degree(Poly p) {
  return p[0];
}

int poly_compare(Poly p1, Poly p2)
{
  int i, l1, l2;
  l1 = p1[0];
  l2 = p2[0];
  
  if (l1 < l2)
    return -1;
  if (l1 > l2)
    return 1;

  for (i=l1; i>-1; i--)
    if (p1[1+i] < p2[1+i])
      return -1;
    else
      if (p1[1+i] > p2[1+i])
        return 1;
  
  return 0;
}

int poly_asm_compare(Poly p1, Poly p2) {
  int l1, l2;
  l1 = p1[0];
  l2 = p2[0];

  if (l1 < l2)
    return -1;
  if (l1 > l2)
    return 1;
  return 0;
}

int poly_asm_compare_int(Poly p1, int l2)
{
  int l1;
  l1 = p1[0];
  if (l1 < l2)
    return -1;
  if (l1 > l2)
    return 1;
  return 0;
}

int poly_compare_int(Poly p, Factor i)
{
  if (p[0] > 0)
    return 1;
  if (p[1] > i)
    return 1;
  if (p[1] < i)
    return -1;
  return 0;
}

void poly_copy_free(Poly *p1, Poly *p2)
{
  Poly t;

  t = *p1;
  *p1 = *p2;
  free(t);
}


double poly_insert(Poly p, int n)
{
  int i;
  int l = p[0];
  double r = 0;
  Poly s = p + 1;
  for (i=0; i<=l; i++)
    r += s[i] * pow(n,i);
  return r;
}
