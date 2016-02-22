package ADPC;


public class IntLoop11 {

  // 1 byte prefix is assumed
  static byte[] s;

  // alphabet size (A,C,G,U,N)
  static final int ASIZE = 5;
  static final int A = 0;
  static final int C = 1;
  static final int G = 2;
  static final int U = 3;
  static final int N = 4;

  static int intloop11[][][][][][] = 
    new int[ASIZE+1][ASIZE+1][ASIZE+1][ASIZE+1][ASIZE+1][ASIZE+1];

/* ---------------------------------------------------------------------------
RNAshapes
Copyright (C) 2005-2006 Peter Steffen, Bjoern Voss, Jens Reeder,
Marc Rehmsmeier, Robert Giegerich.
------------------------------------------------------------------------------
Intloop11.c
RNA energy library, based on Haskell implementation by Jens Reeder.

The energy parameters are taken from:
  Mathews DH, Sabina J, Zuker M, Turner DH.  Expanded sequence
  dependence of thermodynamic parameters improves prediction of RNA
  secondary structure, J Mol Biol., 288 (5), pp 911-940, 1999

Author: Peter Steffen
$Date: 2006/01/04 16:47:33 $
--------------------------------------------------------------------------- */

/*
Data arrangement: internal loop formed by b,e framed by pairs (a,f) and (c,d)

     5'.... a  b  c.......
            |     |      . 
     3'.... f  e  d ......

      _________
     |    _    |
     |   | |   |
     a b c d e f 
*/


  public static int il11_energy(int lb, int rb)
  {
    return intloop11[s[lb]][s[lb+1]][s[lb+2]][s[rb-2]][s[rb-1]][s[rb]];
  }

  public static void init_intloop11(byte[] z)
  {
    s = z;
    
    int k1,k2,k3,k4,k5,k6;

    /* set to maximal value */
    for (k1=0;k1<ASIZE+1;k1++)
      for (k2=0;k2<ASIZE+1;k2++)
        for (k3=0;k3<ASIZE+1;k3++)
          for (k4=0;k4<ASIZE+1;k4++)
            for (k5=0;k5<ASIZE+1;k5++)
              for (k6=0;k6<ASIZE+1;k6++) {
                if (((k1 == G) && (k6 == C)) ||
                    ((k1 == C) && (k6 == G)) ||
                    ((k3 == G) && (k4 == C)) ||
                    ((k3 == C) && (k4 == G)))
                  intloop11[k1][k2][k3][k4][k5][k6]=110;
                else       intloop11[k1][k2][k3][k4][k5][k6]=170;

              }

    intloop11[C][A][G][C][A][G] = 110 ;
    intloop11[C][A][G][C][C][G] = 40 ;
    intloop11[C][A][G][C][G][G] = 40 ;
    intloop11[C][A][G][C][U][G] = 40 ;
    intloop11[C][C][G][C][A][G] = 40 ;
    intloop11[C][C][G][C][C][G] = 40 ;
    intloop11[C][C][G][C][G][G] = 40 ;
    intloop11[C][C][G][C][U][G] = 40 ;
    intloop11[C][G][G][C][A][G] = 40 ;
    intloop11[C][G][G][C][C][G] = 40 ;
    intloop11[C][G][G][C][G][G] = -140 ;
    intloop11[C][G][G][C][U][G] = 40 ;
    intloop11[C][U][G][C][A][G] = 40 ;
    intloop11[C][U][G][C][C][G] = 40 ;
    intloop11[C][U][G][C][G][G] = 40 ;
    intloop11[C][U][G][C][U][G] = 40 ;
    intloop11[C][A][C][G][A][G] = 40 ;
    intloop11[C][A][C][G][C][G] = -40 ;
    intloop11[C][A][C][G][G][G] = 40 ;
    intloop11[C][A][C][G][U][G] = 40 ;
    intloop11[C][C][C][G][A][G] = 30 ;
    intloop11[C][C][C][G][C][G] = 50 ;
    intloop11[C][C][C][G][G][G] = 40 ;
    intloop11[C][C][C][G][U][G] = 50 ;
    intloop11[C][G][C][G][A][G] = -10 ;
    intloop11[C][G][C][G][C][G] = 40 ;
    intloop11[C][G][C][G][G][G] = -170 ;
    intloop11[C][G][C][G][U][G] = 40 ;
    intloop11[C][U][C][G][A][G] = 40 ;
    intloop11[C][U][C][G][C][G] = 0 ;
    intloop11[C][U][C][G][G][G] = 40 ;
    intloop11[C][U][C][G][U][G] = -30 ;
    intloop11[C][A][U][G][A][G] = 110 ;
    intloop11[C][A][U][G][C][G] = 110 ;
    intloop11[C][A][U][G][G][G] = 110 ;
    intloop11[C][A][U][G][U][G] = 110 ;
    intloop11[C][C][U][G][A][G] = 110 ;
    intloop11[C][C][U][G][C][G] = 110 ;
    intloop11[C][C][U][G][G][G] = 110 ;
    intloop11[C][C][U][G][U][G] = 110 ;
    intloop11[C][G][U][G][A][G] = 110 ;
    intloop11[C][G][U][G][C][G] = 110 ;
    intloop11[C][G][U][G][G][G] = -100 ;
    intloop11[C][G][U][G][U][G] = 110 ;
    intloop11[C][U][U][G][A][G] = 110 ;
    intloop11[C][U][U][G][C][G] = 110 ;
    intloop11[C][U][U][G][G][G] = 110 ;
    intloop11[C][U][U][G][U][G] = 110 ;
    intloop11[C][A][G][U][A][G] = 110 ;
    intloop11[C][A][G][U][C][G] = 110 ;
    intloop11[C][A][G][U][G][G] = 110 ;
    intloop11[C][A][G][U][U][G] = 110 ;
    intloop11[C][C][G][U][A][G] = 110 ;
    intloop11[C][C][G][U][C][G] = 110 ;
    intloop11[C][C][G][U][G][G] = 110 ;
    intloop11[C][C][G][U][U][G] = 110 ;
    intloop11[C][G][G][U][A][G] = 110 ;
    intloop11[C][G][G][U][C][G] = 110 ;
    intloop11[C][G][G][U][G][G] = -100 ;
    intloop11[C][G][G][U][U][G] = 110 ;
    intloop11[C][U][G][U][A][G] = 110 ;
    intloop11[C][U][G][U][C][G] = 110 ;
    intloop11[C][U][G][U][G][G] = 110 ;
    intloop11[C][U][G][U][U][G] = 110 ;
    intloop11[C][A][U][A][A][G] = 110 ;
    intloop11[C][A][U][A][C][G] = 110 ;
    intloop11[C][A][U][A][G][G] = 110 ;
    intloop11[C][A][U][A][U][G] = 110 ;
    intloop11[C][C][U][A][A][G] = 110 ;
    intloop11[C][C][U][A][C][G] = 110 ;
    intloop11[C][C][U][A][G][G] = 110 ;
    intloop11[C][C][U][A][U][G] = 110 ;
    intloop11[C][G][U][A][A][G] = 110 ;
    intloop11[C][G][U][A][C][G] = 110 ;
    intloop11[C][G][U][A][G][G] = -100 ;
    intloop11[C][G][U][A][U][G] = 110 ;
    intloop11[C][U][U][A][A][G] = 110 ;
    intloop11[C][U][U][A][C][G] = 110 ;
    intloop11[C][U][U][A][G][G] = 110 ;
    intloop11[C][U][U][A][U][G] = 110 ;
    intloop11[C][A][A][U][A][G] = 110 ;
    intloop11[C][A][A][U][C][G] = 110 ;
    intloop11[C][A][A][U][G][G] = 110 ;
    intloop11[C][A][A][U][U][G] = 110 ;
    intloop11[C][C][A][U][A][G] = 110 ;
    intloop11[C][C][A][U][C][G] = 110 ;
    intloop11[C][C][A][U][G][G] = 110 ;
    intloop11[C][C][A][U][U][G] = 110 ;
    intloop11[C][G][A][U][A][G] = 110 ;
    intloop11[C][G][A][U][C][G] = 110 ;
    intloop11[C][G][A][U][G][G] = -100 ;
    intloop11[C][G][A][U][U][G] = 110 ;
    intloop11[C][U][A][U][A][G] = 110 ;
    intloop11[C][U][A][U][C][G] = 110 ;
    intloop11[C][U][A][U][G][G] = 110 ;
    intloop11[C][U][A][U][U][G] = 110 ;
    intloop11[G][A][G][C][A][C] = 40 ;
    intloop11[G][A][G][C][C][C] = 30 ;
    intloop11[G][A][G][C][G][C] = -10 ;
    intloop11[G][A][G][C][U][C] = 40 ;
    intloop11[G][C][G][C][A][C] = -40 ;
    intloop11[G][C][G][C][C][C] = 50 ;
    intloop11[G][C][G][C][G][C] = 40 ;
    intloop11[G][C][G][C][U][C] = 0 ;
    intloop11[G][G][G][C][A][C] = 40 ;
    intloop11[G][G][G][C][C][C] = 40 ;
    intloop11[G][G][G][C][G][C] = -170 ;
    intloop11[G][G][G][C][U][C] = 40 ;
    intloop11[G][U][G][C][A][C] = 40 ;
    intloop11[G][U][G][C][C][C] = 50 ;
    intloop11[G][U][G][C][G][C] = 40 ;
    intloop11[G][U][G][C][U][C] = -30 ;
    intloop11[G][A][C][G][A][C] = 80 ;
    intloop11[G][A][C][G][C][C] = 40 ;
    intloop11[G][A][C][G][G][C] = 40 ;
    intloop11[G][A][C][G][U][C] = 40 ;
    intloop11[G][C][C][G][A][C] = 40 ;
    intloop11[G][C][C][G][C][C] = 40 ;
    intloop11[G][C][C][G][G][C] = 40 ;
    intloop11[G][C][C][G][U][C] = 40 ;
    intloop11[G][G][C][G][A][C] = 40 ;
    intloop11[G][G][C][G][C][C] = 40 ;
    intloop11[G][G][C][G][G][C] = -210 ;
    intloop11[G][G][C][G][U][C] = 40 ;
    intloop11[G][U][C][G][A][C] = 40 ;
    intloop11[G][U][C][G][C][C] = 40 ;
    intloop11[G][U][C][G][G][C] = 40 ;
    intloop11[G][U][C][G][U][C] = -70 ;
    intloop11[G][A][U][G][A][C] = 110 ;
    intloop11[G][A][U][G][C][C] = 110 ;
    intloop11[G][A][U][G][G][C] = 110 ;
    intloop11[G][A][U][G][U][C] = 110 ;
    intloop11[G][C][U][G][A][C] = 110 ;
    intloop11[G][C][U][G][C][C] = 110 ;
    intloop11[G][C][U][G][G][C] = 110 ;
    intloop11[G][C][U][G][U][C] = 110 ;
    intloop11[G][G][U][G][A][C] = 110 ;
    intloop11[G][G][U][G][C][C] = 110 ;
    intloop11[G][G][U][G][G][C] = -100 ;
    intloop11[G][G][U][G][U][C] = 110 ;
    intloop11[G][U][U][G][A][C] = 110 ;
    intloop11[G][U][U][G][C][C] = 110 ;
    intloop11[G][U][U][G][G][C] = 110 ;
    intloop11[G][U][U][G][U][C] = 110 ;
    intloop11[G][A][G][U][A][C] = 110 ;
    intloop11[G][A][G][U][C][C] = 110 ;
    intloop11[G][A][G][U][G][C] = 110 ;
    intloop11[G][A][G][U][U][C] = 110 ;
    intloop11[G][C][G][U][A][C] = 110 ;
    intloop11[G][C][G][U][C][C] = 110 ;
    intloop11[G][C][G][U][G][C] = 110 ;
    intloop11[G][C][G][U][U][C] = 110 ;
    intloop11[G][G][G][U][A][C] = 110 ;
    intloop11[G][G][G][U][C][C] = 110 ;
    intloop11[G][G][G][U][G][C] = -100 ;
    intloop11[G][G][G][U][U][C] = 110 ;
    intloop11[G][U][G][U][A][C] = 110 ;
    intloop11[G][U][G][U][C][C] = 110 ;
    intloop11[G][U][G][U][G][C] = 110 ;
    intloop11[G][U][G][U][U][C] = 110 ;
    intloop11[G][A][U][A][A][C] = 110 ;
    intloop11[G][A][U][A][C][C] = 110 ;
    intloop11[G][A][U][A][G][C] = 110 ;
    intloop11[G][A][U][A][U][C] = 110 ;
    intloop11[G][C][U][A][A][C] = 110 ;
    intloop11[G][C][U][A][C][C] = 110 ;
    intloop11[G][C][U][A][G][C] = 110 ;
    intloop11[G][C][U][A][U][C] = 110 ;
    intloop11[G][G][U][A][A][C] = 110 ;
    intloop11[G][G][U][A][C][C] = 110 ;
    intloop11[G][G][U][A][G][C] = -100 ;
    intloop11[G][G][U][A][U][C] = 110 ;
    intloop11[G][U][U][A][A][C] = 110 ;
    intloop11[G][U][U][A][C][C] = 110 ;
    intloop11[G][U][U][A][G][C] = 110 ;
    intloop11[G][U][U][A][U][C] = 100 ;
    intloop11[G][A][A][U][A][C] = 110 ;
    intloop11[G][A][A][U][C][C] = 110 ;
    intloop11[G][A][A][U][G][C] = 110 ;
    intloop11[G][A][A][U][U][C] = 110 ;
    intloop11[G][C][A][U][A][C] = 110 ;
    intloop11[G][C][A][U][C][C] = 110 ;
    intloop11[G][C][A][U][G][C] = 110 ;
    intloop11[G][C][A][U][U][C] = 110 ;
    intloop11[G][G][A][U][A][C] = 110 ;
    intloop11[G][G][A][U][C][C] = 110 ;
    intloop11[G][G][A][U][G][C] = -100 ;
    intloop11[G][G][A][U][U][C] = 110 ;
    intloop11[G][U][A][U][A][C] = 110 ;
    intloop11[G][U][A][U][C][C] = 110 ;
    intloop11[G][U][A][U][G][C] = 110 ;
    intloop11[G][U][A][U][U][C] = 110 ;
    intloop11[G][A][G][C][A][U] = 110 ;
    intloop11[G][A][G][C][C][U] = 110 ;
    intloop11[G][A][G][C][G][U] = 110 ;
    intloop11[G][A][G][C][U][U] = 110 ;
    intloop11[G][C][G][C][A][U] = 110 ;
    intloop11[G][C][G][C][C][U] = 110 ;
    intloop11[G][C][G][C][G][U] = 110 ;
    intloop11[G][C][G][C][U][U] = 110 ;
    intloop11[G][G][G][C][A][U] = 110 ;
    intloop11[G][G][G][C][C][U] = 110 ;
    intloop11[G][G][G][C][G][U] = -100 ;
    intloop11[G][G][G][C][U][U] = 110 ;
    intloop11[G][U][G][C][A][U] = 110 ;
    intloop11[G][U][G][C][C][U] = 110 ;
    intloop11[G][U][G][C][G][U] = 110 ;
    intloop11[G][U][G][C][U][U] = 110 ;
    intloop11[G][A][C][G][A][U] = 110 ;
    intloop11[G][A][C][G][C][U] = 110 ;
    intloop11[G][A][C][G][G][U] = 110 ;
    intloop11[G][A][C][G][U][U] = 110 ;
    intloop11[G][C][C][G][A][U] = 110 ;
    intloop11[G][C][C][G][C][U] = 110 ;
    intloop11[G][C][C][G][G][U] = 110 ;
    intloop11[G][C][C][G][U][U] = 110 ;
    intloop11[G][G][C][G][A][U] = 110 ;
    intloop11[G][G][C][G][C][U] = 110 ;
    intloop11[G][G][C][G][G][U] = -100 ;
    intloop11[G][G][C][G][U][U] = 110 ;
    intloop11[G][U][C][G][A][U] = 110 ;
    intloop11[G][U][C][G][C][U] = 110 ;
    intloop11[G][U][C][G][G][U] = 110 ;
    intloop11[G][U][C][G][U][U] = 110 ;
    intloop11[G][A][U][G][A][U] = 170 ;
    intloop11[G][A][U][G][C][U] = 170 ;
    intloop11[G][A][U][G][G][U] = 170 ;
    intloop11[G][A][U][G][U][U] = 170 ;
    intloop11[G][C][U][G][A][U] = 170 ;
    intloop11[G][C][U][G][C][U] = 170 ;
    intloop11[G][C][U][G][G][U] = 170 ;
    intloop11[G][C][U][G][U][U] = 170 ;
    intloop11[G][G][U][G][A][U] = 170 ;
    intloop11[G][G][U][G][C][U] = 170 ;
    intloop11[G][G][U][G][G][U] = -40 ;
    intloop11[G][G][U][G][U][U] = 170 ;
    intloop11[G][U][U][G][A][U] = 170 ;
    intloop11[G][U][U][G][C][U] = 170 ;
    intloop11[G][U][U][G][G][U] = 170 ;
    intloop11[G][U][U][G][U][U] = 170 ;
    intloop11[G][A][G][U][A][U] = 170 ;
    intloop11[G][A][G][U][C][U] = 170 ;
    intloop11[G][A][G][U][G][U] = 170 ;
    intloop11[G][A][G][U][U][U] = 170 ;
    intloop11[G][C][G][U][A][U] = 170 ;
    intloop11[G][C][G][U][C][U] = 170 ;
    intloop11[G][C][G][U][G][U] = 170 ;
    intloop11[G][C][G][U][U][U] = 170 ;
    intloop11[G][G][G][U][A][U] = 170 ;
    intloop11[G][G][G][U][C][U] = 170 ;
    intloop11[G][G][G][U][G][U] = -40 ;
    intloop11[G][G][G][U][U][U] = 170 ;
    intloop11[G][U][G][U][A][U] = 170 ;
    intloop11[G][U][G][U][C][U] = 170 ;
    intloop11[G][U][G][U][G][U] = 170 ;
    intloop11[G][U][G][U][U][U] = 170 ;
    intloop11[G][A][U][A][A][U] = 170 ;
    intloop11[G][A][U][A][C][U] = 170 ;
    intloop11[G][A][U][A][G][U] = 170 ;
    intloop11[G][A][U][A][U][U] = 170 ;
    intloop11[G][C][U][A][A][U] = 170 ;
    intloop11[G][C][U][A][C][U] = 170 ;
    intloop11[G][C][U][A][G][U] = 170 ;
    intloop11[G][C][U][A][U][U] = 170 ;
    intloop11[G][G][U][A][A][U] = 170 ;
    intloop11[G][G][U][A][C][U] = 170 ;
    intloop11[G][G][U][A][G][U] = -40 ;
    intloop11[G][G][U][A][U][U] = 170 ;
    intloop11[G][U][U][A][A][U] = 170 ;
    intloop11[G][U][U][A][C][U] = 170 ;
    intloop11[G][U][U][A][G][U] = 170 ;
    intloop11[G][U][U][A][U][U] = 170 ;
    intloop11[G][A][A][U][A][U] = 170 ;
    intloop11[G][A][A][U][C][U] = 170 ;
    intloop11[G][A][A][U][G][U] = 170 ;
    intloop11[G][A][A][U][U][U] = 170 ;
    intloop11[G][C][A][U][A][U] = 170 ;
    intloop11[G][C][A][U][C][U] = 170 ;
    intloop11[G][C][A][U][G][U] = 170 ;
    intloop11[G][C][A][U][U][U] = 170 ;
    intloop11[G][G][A][U][A][U] = 170 ;
    intloop11[G][G][A][U][C][U] = 170 ;
    intloop11[G][G][A][U][G][U] = -40 ;
    intloop11[G][G][A][U][U][U] = 170 ;
    intloop11[G][U][A][U][A][U] = 170 ;
    intloop11[G][U][A][U][C][U] = 170 ;
    intloop11[G][U][A][U][G][U] = 170 ;
    intloop11[G][U][A][U][U][U] = 170 ;
    intloop11[U][A][G][C][A][G] = 110 ;
    intloop11[U][A][G][C][C][G] = 110 ;
    intloop11[U][A][G][C][G][G] = 110 ;
    intloop11[U][A][G][C][U][G] = 110 ;
    intloop11[U][C][G][C][A][G] = 110 ;
    intloop11[U][C][G][C][C][G] = 110 ;
    intloop11[U][C][G][C][G][G] = 110 ;
    intloop11[U][C][G][C][U][G] = 110 ;
    intloop11[U][G][G][C][A][G] = 110 ;
    intloop11[U][G][G][C][C][G] = 110 ;
    intloop11[U][G][G][C][G][G] = -100 ;
    intloop11[U][G][G][C][U][G] = 110 ;
    intloop11[U][U][G][C][A][G] = 110 ;
    intloop11[U][U][G][C][C][G] = 110 ;
    intloop11[U][U][G][C][G][G] = 110 ;
    intloop11[U][U][G][C][U][G] = 110 ;
    intloop11[U][A][C][G][A][G] = 110 ;
    intloop11[U][A][C][G][C][G] = 110 ;
    intloop11[U][A][C][G][G][G] = 110 ;
    intloop11[U][A][C][G][U][G] = 110 ;
    intloop11[U][C][C][G][A][G] = 110 ;
    intloop11[U][C][C][G][C][G] = 110 ;
    intloop11[U][C][C][G][G][G] = 110 ;
    intloop11[U][C][C][G][U][G] = 110 ;
    intloop11[U][G][C][G][A][G] = 110 ;
    intloop11[U][G][C][G][C][G] = 110 ;
    intloop11[U][G][C][G][G][G] = -100 ;
    intloop11[U][G][C][G][U][G] = 110 ;
    intloop11[U][U][C][G][A][G] = 110 ;
    intloop11[U][U][C][G][C][G] = 110 ;
    intloop11[U][U][C][G][G][G] = 110 ;
    intloop11[U][U][C][G][U][G] = 110 ;
    intloop11[U][A][U][G][A][G] = 170 ;
    intloop11[U][A][U][G][C][G] = 170 ;
    intloop11[U][A][U][G][G][G] = 170 ;
    intloop11[U][A][U][G][U][G] = 170 ;
    intloop11[U][C][U][G][A][G] = 170 ;
    intloop11[U][C][U][G][C][G] = 170 ;
    intloop11[U][C][U][G][G][G] = 170 ;
    intloop11[U][C][U][G][U][G] = 170 ;
    intloop11[U][G][U][G][A][G] = 170 ;
    intloop11[U][G][U][G][C][G] = 170 ;
    intloop11[U][G][U][G][G][G] = -40 ;
    intloop11[U][G][U][G][U][G] = 170 ;
    intloop11[U][U][U][G][A][G] = 170 ;
    intloop11[U][U][U][G][C][G] = 170 ;
    intloop11[U][U][U][G][G][G] = 170 ;
    intloop11[U][U][U][G][U][G] = 170 ;
    intloop11[U][A][G][U][A][G] = 170 ;
    intloop11[U][A][G][U][C][G] = 170 ;
    intloop11[U][A][G][U][G][G] = 170 ;
    intloop11[U][A][G][U][U][G] = 170 ;
    intloop11[U][C][G][U][A][G] = 170 ;
    intloop11[U][C][G][U][C][G] = 170 ;
    intloop11[U][C][G][U][G][G] = 170 ;
    intloop11[U][C][G][U][U][G] = 170 ;
    intloop11[U][G][G][U][A][G] = 170 ;
    intloop11[U][G][G][U][C][G] = 170 ;
    intloop11[U][G][G][U][G][G] = -40 ;
    intloop11[U][G][G][U][U][G] = 170 ;
    intloop11[U][U][G][U][A][G] = 170 ;
    intloop11[U][U][G][U][C][G] = 170 ;
    intloop11[U][U][G][U][G][G] = 170 ;
    intloop11[U][U][G][U][U][G] = 170 ;
    intloop11[U][A][U][A][A][G] = 170 ;
    intloop11[U][A][U][A][C][G] = 170 ;
    intloop11[U][A][U][A][G][G] = 170 ;
    intloop11[U][A][U][A][U][G] = 170 ;
    intloop11[U][C][U][A][A][G] = 170 ;
    intloop11[U][C][U][A][C][G] = 170 ;
    intloop11[U][C][U][A][G][G] = 170 ;
    intloop11[U][C][U][A][U][G] = 170 ;
    intloop11[U][G][U][A][A][G] = 170 ;
    intloop11[U][G][U][A][C][G] = 170 ;
    intloop11[U][G][U][A][G][G] = -40 ;
    intloop11[U][G][U][A][U][G] = 170 ;
    intloop11[U][U][U][A][A][G] = 170 ;
    intloop11[U][U][U][A][C][G] = 170 ;
    intloop11[U][U][U][A][G][G] = 170 ;
    intloop11[U][U][U][A][U][G] = 170 ;
    intloop11[U][A][A][U][A][G] = 170 ;
    intloop11[U][A][A][U][C][G] = 170 ;
    intloop11[U][A][A][U][G][G] = 170 ;
    intloop11[U][A][A][U][U][G] = 170 ;
    intloop11[U][C][A][U][A][G] = 170 ;
    intloop11[U][C][A][U][C][G] = 170 ;
    intloop11[U][C][A][U][G][G] = 170 ;
    intloop11[U][C][A][U][U][G] = 170 ;
    intloop11[U][G][A][U][A][G] = 170 ;
    intloop11[U][G][A][U][C][G] = 170 ;
    intloop11[U][G][A][U][G][G] = -40 ;
    intloop11[U][G][A][U][U][G] = 170 ;
    intloop11[U][U][A][U][A][G] = 170 ;
    intloop11[U][U][A][U][C][G] = 170 ;
    intloop11[U][U][A][U][G][G] = 170 ;
    intloop11[U][U][A][U][U][G] = 170 ;
    intloop11[A][A][G][C][A][U] = 110 ;
    intloop11[A][A][G][C][C][U] = 110 ;
    intloop11[A][A][G][C][G][U] = 110 ;
    intloop11[A][A][G][C][U][U] = 110 ;
    intloop11[A][C][G][C][A][U] = 110 ;
    intloop11[A][C][G][C][C][U] = 110 ;
    intloop11[A][C][G][C][G][U] = 110 ;
    intloop11[A][C][G][C][U][U] = 110 ;
    intloop11[A][G][G][C][A][U] = 110 ;
    intloop11[A][G][G][C][C][U] = 110 ;
    intloop11[A][G][G][C][G][U] = -100 ;
    intloop11[A][G][G][C][U][U] = 110 ;
    intloop11[A][U][G][C][A][U] = 110 ;
    intloop11[A][U][G][C][C][U] = 110 ;
    intloop11[A][U][G][C][G][U] = 110 ;
    intloop11[A][U][G][C][U][U] = 110 ;
    intloop11[A][A][C][G][A][U] = 110 ;
    intloop11[A][A][C][G][C][U] = 110 ;
    intloop11[A][A][C][G][G][U] = 110 ;
    intloop11[A][A][C][G][U][U] = 110 ;
    intloop11[A][C][C][G][A][U] = 110 ;
    intloop11[A][C][C][G][C][U] = 110 ;
    intloop11[A][C][C][G][G][U] = 110 ;
    intloop11[A][C][C][G][U][U] = 110 ;
    intloop11[A][G][C][G][A][U] = 110 ;
    intloop11[A][G][C][G][C][U] = 110 ;
    intloop11[A][G][C][G][G][U] = -100 ;
    intloop11[A][G][C][G][U][U] = 110 ;
    intloop11[A][U][C][G][A][U] = 110 ;
    intloop11[A][U][C][G][C][U] = 110 ;
    intloop11[A][U][C][G][G][U] = 110 ;
    intloop11[A][U][C][G][U][U] = 100 ;
    intloop11[A][A][U][G][A][U] = 170 ;
    intloop11[A][A][U][G][C][U] = 170 ;
    intloop11[A][A][U][G][G][U] = 170 ;
    intloop11[A][A][U][G][U][U] = 170 ;
    intloop11[A][C][U][G][A][U] = 170 ;
    intloop11[A][C][U][G][C][U] = 170 ;
    intloop11[A][C][U][G][G][U] = 170 ;
    intloop11[A][C][U][G][U][U] = 170 ;
    intloop11[A][G][U][G][A][U] = 170 ;
    intloop11[A][G][U][G][C][U] = 170 ;
    intloop11[A][G][U][G][G][U] = -40 ;
    intloop11[A][G][U][G][U][U] = 170 ;
    intloop11[A][U][U][G][A][U] = 170 ;
    intloop11[A][U][U][G][C][U] = 170 ;
    intloop11[A][U][U][G][G][U] = 170 ;
    intloop11[A][U][U][G][U][U] = 170 ;
    intloop11[A][A][G][U][A][U] = 170 ;
    intloop11[A][A][G][U][C][U] = 170 ;
    intloop11[A][A][G][U][G][U] = 170 ;
    intloop11[A][A][G][U][U][U] = 170 ;
    intloop11[A][C][G][U][A][U] = 170 ;
    intloop11[A][C][G][U][C][U] = 170 ;
    intloop11[A][C][G][U][G][U] = 170 ;
    intloop11[A][C][G][U][U][U] = 170 ;
    intloop11[A][G][G][U][A][U] = 170 ;
    intloop11[A][G][G][U][C][U] = 170 ;
    intloop11[A][G][G][U][G][U] = -40 ;
    intloop11[A][G][G][U][U][U] = 170 ;
    intloop11[A][U][G][U][A][U] = 170 ;
    intloop11[A][U][G][U][C][U] = 170 ;
    intloop11[A][U][G][U][G][U] = 170 ;
    intloop11[A][U][G][U][U][U] = 170 ;
    intloop11[A][A][U][A][A][U] = 170 ;
    intloop11[A][A][U][A][C][U] = 170 ;
    intloop11[A][A][U][A][G][U] = 170 ;
    intloop11[A][A][U][A][U][U] = 170 ;
    intloop11[A][C][U][A][A][U] = 170 ;
    intloop11[A][C][U][A][C][U] = 170 ;
    intloop11[A][C][U][A][G][U] = 170 ;
    intloop11[A][C][U][A][U][U] = 170 ;
    intloop11[A][G][U][A][A][U] = 170 ;
    intloop11[A][G][U][A][C][U] = 170 ;
    intloop11[A][G][U][A][G][U] = -40 ;
    intloop11[A][G][U][A][U][U] = 170 ;
    intloop11[A][U][U][A][A][U] = 170 ;
    intloop11[A][U][U][A][C][U] = 170 ;
    intloop11[A][U][U][A][G][U] = 170 ;
    intloop11[A][U][U][A][U][U] = 120 ;
    intloop11[A][A][A][U][A][U] = 170 ;
    intloop11[A][A][A][U][C][U] = 170 ;
    intloop11[A][A][A][U][G][U] = 170 ;
    intloop11[A][A][A][U][U][U] = 170 ;
    intloop11[A][C][A][U][A][U] = 170 ;
    intloop11[A][C][A][U][C][U] = 170 ;
    intloop11[A][C][A][U][G][U] = 170 ;
    intloop11[A][C][A][U][U][U] = 170 ;
    intloop11[A][G][A][U][A][U] = 170 ;
    intloop11[A][G][A][U][C][U] = 170 ;
    intloop11[A][G][A][U][G][U] = -40 ;
    intloop11[A][G][A][U][U][U] = 170 ;
    intloop11[A][U][A][U][A][U] = 170 ;
    intloop11[A][U][A][U][C][U] = 170 ;
    intloop11[A][U][A][U][G][U] = 170 ;
    intloop11[A][U][A][U][U][U] = 150 ;
    intloop11[U][A][G][C][A][A] = 110 ;
    intloop11[U][A][G][C][C][A] = 110 ;
    intloop11[U][A][G][C][G][A] = 110 ;
    intloop11[U][A][G][C][U][A] = 110 ;
    intloop11[U][C][G][C][A][A] = 110 ;
    intloop11[U][C][G][C][C][A] = 110 ;
    intloop11[U][C][G][C][G][A] = 110 ;
    intloop11[U][C][G][C][U][A] = 110 ;
    intloop11[U][G][G][C][A][A] = 110 ;
    intloop11[U][G][G][C][C][A] = 110 ;
    intloop11[U][G][G][C][G][A] = -100 ;
    intloop11[U][G][G][C][U][A] = 110 ;
    intloop11[U][U][G][C][A][A] = 110 ;
    intloop11[U][U][G][C][C][A] = 110 ;
    intloop11[U][U][G][C][G][A] = 110 ;
    intloop11[U][U][G][C][U][A] = 110 ;
    intloop11[U][A][C][G][A][A] = 110 ;
    intloop11[U][A][C][G][C][A] = 110 ;
    intloop11[U][A][C][G][G][A] = 110 ;
    intloop11[U][A][C][G][U][A] = 110 ;
    intloop11[U][C][C][G][A][A] = 110 ;
    intloop11[U][C][C][G][C][A] = 110 ;
    intloop11[U][C][C][G][G][A] = 110 ;
    intloop11[U][C][C][G][U][A] = 110 ;
    intloop11[U][G][C][G][A][A] = 110 ;
    intloop11[U][G][C][G][C][A] = 110 ;
    intloop11[U][G][C][G][G][A] = -100 ;
    intloop11[U][G][C][G][U][A] = 110 ;
    intloop11[U][U][C][G][A][A] = 110 ;
    intloop11[U][U][C][G][C][A] = 110 ;
    intloop11[U][U][C][G][G][A] = 110 ;
    intloop11[U][U][C][G][U][A] = 110 ;
    intloop11[U][A][U][G][A][A] = 170 ;
    intloop11[U][A][U][G][C][A] = 170 ;
    intloop11[U][A][U][G][G][A] = 170 ;
    intloop11[U][A][U][G][U][A] = 170 ;
    intloop11[U][C][U][G][A][A] = 170 ;
    intloop11[U][C][U][G][C][A] = 170 ;
    intloop11[U][C][U][G][G][A] = 170 ;
    intloop11[U][C][U][G][U][A] = 170 ;
    intloop11[U][G][U][G][A][A] = 170 ;
    intloop11[U][G][U][G][C][A] = 170 ;
    intloop11[U][G][U][G][G][A] = -40 ;
    intloop11[U][G][U][G][U][A] = 170 ;
    intloop11[U][U][U][G][A][A] = 170 ;
    intloop11[U][U][U][G][C][A] = 170 ;
    intloop11[U][U][U][G][G][A] = 170 ;
    intloop11[U][U][U][G][U][A] = 170 ;
    intloop11[U][A][G][U][A][A] = 170 ;
    intloop11[U][A][G][U][C][A] = 170 ;
    intloop11[U][A][G][U][G][A] = 170 ;
    intloop11[U][A][G][U][U][A] = 170 ;
    intloop11[U][C][G][U][A][A] = 170 ;
    intloop11[U][C][G][U][C][A] = 170 ;
    intloop11[U][C][G][U][G][A] = 170 ;
    intloop11[U][C][G][U][U][A] = 170 ;
    intloop11[U][G][G][U][A][A] = 170 ;
    intloop11[U][G][G][U][C][A] = 170 ;
    intloop11[U][G][G][U][G][A] = -40 ;
    intloop11[U][G][G][U][U][A] = 170 ;
    intloop11[U][U][G][U][A][A] = 170 ;
    intloop11[U][U][G][U][C][A] = 170 ;
    intloop11[U][U][G][U][G][A] = 170 ;
    intloop11[U][U][G][U][U][A] = 170 ;
    intloop11[U][A][U][A][A][A] = 170 ;
    intloop11[U][A][U][A][C][A] = 170 ;
    intloop11[U][A][U][A][G][A] = 170 ;
    intloop11[U][A][U][A][U][A] = 170 ;
    intloop11[U][C][U][A][A][A] = 170 ;
    intloop11[U][C][U][A][C][A] = 170 ;
    intloop11[U][C][U][A][G][A] = 170 ;
    intloop11[U][C][U][A][U][A] = 170 ;
    intloop11[U][G][U][A][A][A] = 170 ;
    intloop11[U][G][U][A][C][A] = 170 ;
    intloop11[U][G][U][A][G][A] = -40 ;
    intloop11[U][G][U][A][U][A] = 170 ;
    intloop11[U][U][U][A][A][A] = 170 ;
    intloop11[U][U][U][A][C][A] = 170 ;
    intloop11[U][U][U][A][G][A] = 170 ;
    intloop11[U][U][U][A][U][A] = 150 ;
    intloop11[U][A][A][U][A][A] = 170 ;
    intloop11[U][A][A][U][C][A] = 170 ;
    intloop11[U][A][A][U][G][A] = 170 ;
    intloop11[U][A][A][U][U][A] = 170 ;
    intloop11[U][C][A][U][A][A] = 170 ;
    intloop11[U][C][A][U][C][A] = 170 ;
    intloop11[U][C][A][U][G][A] = 170 ;
    intloop11[U][C][A][U][U][A] = 170 ;
    intloop11[U][G][A][U][A][A] = 170 ;
    intloop11[U][G][A][U][C][A] = 170 ;
    intloop11[U][G][A][U][G][A] = -40 ;
    intloop11[U][G][A][U][U][A] = 170 ;
    intloop11[U][U][A][U][A][A] = 170 ;
    intloop11[U][U][A][U][C][A] = 170 ;
    intloop11[U][U][A][U][G][A] = 170 ;
    intloop11[U][U][A][U][U][A] = 180 ;
  }

}