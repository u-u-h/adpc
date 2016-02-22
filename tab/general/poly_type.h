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

#ifndef _POLY_H_
#define _POLY_H_

#include <inttypes.h>

/*! \file poly_type.h
    \brief Some functions to deal with the poly datatype */

/*! \brief Internal representation of a polynom
   
   Ein Polynom der Form 
     \f$x_0 * n^0 + x_1 * n^1 + ... + x_l * n^l\f$
   wird wie folgt gespeichert:
     \f$[l, x_0, x_1, .. ,x_l]\f$
*/
typedef uint64_t Factor;
typedef Factor *Poly;
#define PRIfactor PRIu64


/*! \brief Copies a polynom
    \param s maximal size of polynom
    \param new allocated polynom */
Poly poly_copy(Poly p, int s);

/*! \brief Addition
  
    |p1| == |p2|, result in p1 */
void poly_add(Poly p1, Poly p2);

/*! \brief Subtraction
  
    |p1| == |p2|, result in p1 */
void poly_sub(Poly p1, Poly p2);

/*! \brief Multiplication
   
    size(result) = max(s,|p1| + |p2|)

    \param s maximal size of new allocated polyom
    \return new allocated polynom,
            if degree(result) > s, then n^s returned*/
Poly poly_times(Poly p1, Poly p2, int s);

/*! \brief Returns new allocated polynom, and frees first parameter */
Poly poly_times_free(Poly p1, Poly p2, int s);

/*! \brief Division

    In place, i.e. result is in p1
 */
void poly_div(Poly p1, Poly p2, int s);

/*! \brief Pretty print a polynom
    \param f output stream */
void poly_print(FILE *f, Poly p);

/*! \brief Pretty print a polynom into a string
    \param s destination string
    \param size maximum size to write into s
    \param p source polynom */
void poly_snprint(char *s, size_t size, Poly p);

/*! \brief Returns the polynom n^i
    \return new allocated polynom */ 
Poly poly_n(int i, int s);

/*! \brief Returns a polynom, with "exponential" size, i.e. n^20 ;)
    \return new allocated polynom */
Poly poly_exp();

/*! \brief Tests if polynom has exponential size
    \return true if "exponential", i.e. \f$n^l, l>19\f$ */
int poly_exponential(Poly p);

/*! \brief Returns an illegal polynom
    \return new allocated illegal polynom, i.e. -1
 */
Poly poly_ill();

/*! \brief Tests if polynom is illegal
    \brief true if polynom is illegal, false else
 */
int poly_illegal(Poly p);


/*! \brief Returns a new allocates polynom: \f$i*n^0\f$ */
Poly poly_int(Factor i, int s);

/*! \brief Returns the degree of a polynom */
int poly_degree(Poly p);

/*! \brief Compares two polynoms.
    \return -1, 0, 1 if p1 < p2, p1==p2, p1>p2 */
int poly_compare(Poly p1, Poly p2);

/*! \brief Asymptotical compares two polynoms
    \return <0, 0, >0, if degree(p1) < degree(p2), d(p1) == d(p2), d(p1)>d(p2)
 */
int poly_asm_compare(Poly p1, Poly p2);

/*! \brief Compares asymptotical a polynom and a given degree
    \return <0, 0, >0, if deg(p1)<l, deg(p1)==l, deg(p1)>l */
int poly_asm_compare_int(Poly p1, int l2);

/*! \brief Compares one polynom with an int.
    \param i \f$1 * n^0 \f$ 
    \return -1, 0, 1 if p smaller than, equal, greater than i */
int poly_compare_int(Poly p, Factor i);

/*! \brief Copies p2 to p1 and frees p2 */
void poly_copy_free(Poly *p1, Poly *p2);

double poly_insert(Poly p, int n);

#endif
