

package ADPC;

import static ADPC.IntLoop11.*;
import static ADPC.IntLoop21.*;
import static ADPC.IntLoop22.*;

public class RNA {

  // 1 byte prefix is assumed
  static byte[] s;

  static int n;

  // alphabet size (A,C,G,U,N)
  static final int ASIZE = 5;
  static final int A = 0;
  static final int C = 1;
  static final int G = 2;
  static final int U = 3;
  static final int N = 4;
  static final int UNDEF = 1000000;


  static byte canPair[][] = new byte[ASIZE][ASIZE];

  static void init_canPair()
  {
    for (int i=0;i<ASIZE;i++)
      for(int j=0;j<ASIZE;j++)
        canPair[i][j]=0;

    canPair [A][U] = 1;
    canPair [U][A] = 1; 
    canPair [C][G] = 1; 
    canPair [G][C] = 1; 
    canPair [G][U] = 1; 
    canPair [U][G] = 1;
  }

  public static boolean basepairing(int i, int j)
  {
    return i+1 < j && canPair [s[i+1]] [s[j]] == 1;
  }

  public static boolean stackpairing(int i, int j)
  {
    return i+3 < j && canPair [s[i+1]] [s[j]] == 1
                   && canPair [s[i+2]] [s[j-1]] == 1;
  }

/* 
   Stabilizing energies for canonical basepairs: AU, CG, GU
   Basepairing: Parameters are in 5' 3' order.
   stack_dg a b c d
            ^ ^ ^ ^
            | |_| |
            |_____| 
*/

  static int stack_dg[][][][] = new int[ASIZE][ASIZE][ASIZE][ASIZE];

  static void init_stack_dg()
  {
    /* report errors by unrealistic values: */
    for(int i=0;i<ASIZE;i++)
      for(int j=0;j<ASIZE;j++)
        for(int k=0;k<ASIZE;k++)
          for(int l=0;l<ASIZE;l++)
            stack_dg[i][j][k][l] = UNDEF;

    stack_dg[C][G][C][G] =  -240 ;
    stack_dg[C][C][G][G] =  -330 ;
    stack_dg[C][U][G][G] =  -210 ;
    stack_dg[C][G][U][G] =  -140 ;
    stack_dg[C][U][A][G] =  -210 ;
    stack_dg[C][A][U][G] =  -210 ;
    stack_dg[G][G][C][C] =  -330 ;
    stack_dg[G][C][G][C] =  -340 ;
    stack_dg[G][U][G][C] =  -250 ;
    stack_dg[G][G][U][C] =  -150 ;
    stack_dg[G][U][A][C] =  -220 ;
    stack_dg[G][A][U][C] =  -240 ;
    stack_dg[G][G][C][U] =  -210 ;
    stack_dg[G][C][G][U] =  -250 ;
    stack_dg[G][U][G][U] =   130 ;
    stack_dg[G][G][U][U] =   -50 ;
    stack_dg[G][U][A][U] =  -140 ;
    stack_dg[G][A][U][U] =  -130 ;
    stack_dg[U][G][C][G] =  -140 ;
    stack_dg[U][C][G][G] =  -150 ;
    stack_dg[U][U][G][G] =   -50 ;
    stack_dg[U][G][U][G] =    30 ;
    stack_dg[U][U][A][G] =   -60 ;
    stack_dg[U][A][U][G] =  -100 ;
    stack_dg[A][G][C][U] =  -210 ;
    stack_dg[A][C][G][U] =  -220 ;
    stack_dg[A][U][G][U] =  -140 ;
    stack_dg[A][G][U][U] =   -60 ;
    stack_dg[A][U][A][U] =  -110 ;
    stack_dg[A][A][U][U] =   -90 ;
    stack_dg[U][G][C][A] =  -210 ;
    stack_dg[U][C][G][A] =  -240 ;
    stack_dg[U][U][G][A] =  -130 ;
    stack_dg[U][G][U][A] =  -100 ;
    stack_dg[U][U][A][A] =   -90 ;
    stack_dg[U][A][U][A] =  -130 ;

  }
  
  public static int sr_energy(int i, int j)
  {
    return stack_dg [s[i]] [s[i+1]] [s[j-1]] [s[j]];
  }

  static int jacobson_stockmayer(int size)
  {
    return (int) (107.856*Math.log((size)/30.0));
  }

  static int hl_ent_ar[];

  static void init_hl_ent_ar()
  {
    hl_ent_ar = new int[Math.max(31, n+1)];

    /* report errors by unrealistic values: */
    hl_ent_ar[0] = UNDEF;
    hl_ent_ar[1] = UNDEF;
    hl_ent_ar[2] = UNDEF;

    hl_ent_ar[3] = 570 ;
    hl_ent_ar[4] = 560 ;
    hl_ent_ar[5] = 560 ;
    hl_ent_ar[6] = 540 ;
    hl_ent_ar[7] = 590 ;
    hl_ent_ar[8] = 560 ;
    hl_ent_ar[9] = 640 ;
    hl_ent_ar[10] = 650 ;
    hl_ent_ar[11] = 660 ;
    hl_ent_ar[12] = 670 ;
    hl_ent_ar[13] = 678 ;
    hl_ent_ar[14] = 686 ;
    hl_ent_ar[15] = 694 ;
    hl_ent_ar[16] = 701 ;
    hl_ent_ar[17] = 707 ;
    hl_ent_ar[18] = 713 ;
    hl_ent_ar[19] = 719 ;
    hl_ent_ar[20] = 725 ;
    hl_ent_ar[21] = 730 ;
    hl_ent_ar[22] = 735 ;
    hl_ent_ar[23] = 740 ;
    hl_ent_ar[24] = 744 ;
    hl_ent_ar[25] = 749 ;
    hl_ent_ar[26] = 753 ;
    hl_ent_ar[27] = 757 ;
    hl_ent_ar[28] = 761 ;
    hl_ent_ar[29] = 765 ;
    hl_ent_ar[30] = 769 ;

    for (int i=31;i<=n;i++)
      hl_ent_ar[i] = hl_ent_ar[30] + jacobson_stockmayer(i);
  }


  static int hl_ent(int i)
  {
    return hl_ent_ar[i];
  }

  static int tstackh_dg[][][][] = new int[ASIZE][ASIZE][ASIZE][ASIZE];

  static void init_tstackh_dg()
  {
    int i,j,k,l;
    /* set to maximal value: */
    for(i=0;i<ASIZE;i++)
      for(j=0;j<ASIZE;j++)
        for(k=0;k<ASIZE;k++)
          for(l=0;l<ASIZE;l++)
            tstackh_dg[i][j][k][l] = 0;

    for(i=0;i<ASIZE-1;i++)
      tstackh_dg[C][i][N][G] = -90;
    tstackh_dg[C][N][N][G] = 0;
    for(i=0;i<ASIZE-1;i++)
      tstackh_dg[G][i][N][C] = -70;
    tstackh_dg[G][N][N][C] = 0;

    tstackh_dg[C][A][A][G] = -150 ;
    tstackh_dg[C][A][C][G] = -150 ;
    tstackh_dg[C][A][G][G] = -140 ;
    tstackh_dg[C][A][U][G] = -180 ;
    tstackh_dg[C][C][A][G] = -100 ;
    tstackh_dg[C][C][C][G] =  -90 ;
    tstackh_dg[C][C][G][G] = -290 ;
    tstackh_dg[C][C][U][G] =  -80 ;
    tstackh_dg[C][G][A][G] = -220 ;
    tstackh_dg[C][G][C][G] = -200 ;
    tstackh_dg[C][G][G][G] = -160 ;
    tstackh_dg[C][G][U][G] = -110 ;
    tstackh_dg[C][U][A][G] = -170 ;
    tstackh_dg[C][U][C][G] = -140 ;
    tstackh_dg[C][U][G][G] = -180 ;
    tstackh_dg[C][U][U][G] = -200 ;
    tstackh_dg[G][A][A][C] = -110 ;
    tstackh_dg[G][A][C][C] = -150 ;
    tstackh_dg[G][A][G][C] = -130 ;
    tstackh_dg[G][A][U][C] = -210 ;
    tstackh_dg[G][C][A][C] = -110 ;
    tstackh_dg[G][C][C][C] =  -70 ;
    tstackh_dg[G][C][G][C] = -240 ;
    tstackh_dg[G][C][U][C] =  -50 ;
    tstackh_dg[G][G][A][C] = -240 ;
    tstackh_dg[G][G][C][C] = -290 ;
    tstackh_dg[G][G][G][C] = -140 ;
    tstackh_dg[G][G][U][C] = -120 ;
    tstackh_dg[G][U][A][C] = -190 ;
    tstackh_dg[G][U][C][C] = -100 ;
    tstackh_dg[G][U][G][C] = -220 ;
    tstackh_dg[G][U][U][C] = -150 ;
    tstackh_dg[G][A][A][U] =   20 ;
    tstackh_dg[G][A][C][U] =  -50 ;
    tstackh_dg[G][A][G][U] =  -30 ;
    tstackh_dg[G][A][U][U] =  -30 ;
    tstackh_dg[G][C][A][U] =  -10 ;
    tstackh_dg[G][C][C][U] =  -20 ;
    tstackh_dg[G][C][G][U] = -150 ;
    tstackh_dg[G][C][U][U] =  -20 ;
    tstackh_dg[G][G][A][U] =  -90 ;
    tstackh_dg[G][G][C][U] = -110 ;
    tstackh_dg[G][G][G][U] =  -30 ;
    tstackh_dg[G][G][U][U] =    0 ;
    tstackh_dg[G][U][A][U] =  -30 ;
    tstackh_dg[G][U][C][U] =  -30 ;
    tstackh_dg[G][U][G][U] =  -40 ;
    tstackh_dg[G][U][U][U] = -110 ;
    tstackh_dg[U][A][A][G] =  -50 ;
    tstackh_dg[U][A][C][G] =  -30 ;
    tstackh_dg[U][A][G][G] =  -60 ;
    tstackh_dg[U][A][U][G] =  -50 ;
    tstackh_dg[U][C][A][G] =  -20 ;
    tstackh_dg[U][C][C][G] =  -10 ;
    tstackh_dg[U][C][G][G] = -170 ;
    tstackh_dg[U][C][U][G] =    0 ;
    tstackh_dg[U][G][A][G] =  -80 ;
    tstackh_dg[U][G][C][G] = -120 ;
    tstackh_dg[U][G][G][G] =  -30 ;
    tstackh_dg[U][G][U][G] =  -70 ;
    tstackh_dg[U][U][A][G] =  -60 ;
    tstackh_dg[U][U][C][G] =  -10 ;
    tstackh_dg[U][U][G][G] =  -60 ;
    tstackh_dg[U][U][U][G] =  -80 ;
    tstackh_dg[A][A][A][U] =  -30 ;
    tstackh_dg[A][A][C][U] =  -50 ;
    tstackh_dg[A][A][G][U] =  -30 ;
    tstackh_dg[A][A][U][U] =  -30 ;
    tstackh_dg[A][C][A][U] =  -10 ;
    tstackh_dg[A][C][C][U] =  -20 ;
    tstackh_dg[A][C][G][U] = -150 ;
    tstackh_dg[A][C][U][U] =  -20 ;
    tstackh_dg[A][G][A][U] = -110 ;
    tstackh_dg[A][G][C][U] = -120 ;
    tstackh_dg[A][G][G][U] =  -20 ;
    tstackh_dg[A][G][U][U] =   20 ;
    tstackh_dg[A][U][A][U] =  -30 ;
    tstackh_dg[A][U][C][U] =  -30 ;
    tstackh_dg[A][U][G][U] =  -60 ;
    tstackh_dg[A][U][U][U] = -110 ;
    tstackh_dg[U][A][A][A] =  -50 ;
    tstackh_dg[U][A][C][A] =  -30 ;
    tstackh_dg[U][A][G][A] =  -60 ;
    tstackh_dg[U][A][U][A] =  -50 ;
    tstackh_dg[U][C][A][A] =  -20 ;
    tstackh_dg[U][C][C][A] =  -10 ;
    tstackh_dg[U][C][G][A] = -120 ;
    tstackh_dg[U][C][U][A] =    0 ;
    tstackh_dg[U][G][A][A] = -140 ;
    tstackh_dg[U][G][C][A] = -120 ;
    tstackh_dg[U][G][G][A] =  -70 ;
    tstackh_dg[U][G][U][A] =  -20 ;
    tstackh_dg[U][U][A][A] =  -30 ;
    tstackh_dg[U][U][C][A] =  -10 ;
    tstackh_dg[U][U][G][A] =  -50 ;
    tstackh_dg[U][U][U][A] =  -80 ;
  }
  
  static int hl_stack(int i, int j)
  {
    return tstackh_dg [s[i]] [s[i+1]] [s[j-1]] [s[j]];
  }

  static int hl_tetra[][][][][][] = 
    new int[ASIZE][ASIZE][ASIZE][ASIZE][ASIZE][ASIZE];
  
  static void init_hl_tetra()
  {
    int k1,k2,k3,k4,k5,k6;

    for (k1=0;k1<ASIZE;k1++)
      for (k2=0;k2<ASIZE;k2++)
        for (k3=0;k3<ASIZE;k3++)
          for (k4=0;k4<ASIZE;k4++)
            for (k5=0;k5<ASIZE;k5++)
              for (k6=0;k6<ASIZE;k6++)
                hl_tetra[k1][k2][k3][k4][k5][k6]=0;

    hl_tetra[G][G][G][G][A][C] = -300 ;
    hl_tetra[G][G][U][G][A][C] = -300 ;
    hl_tetra[C][G][A][A][A][G] = -300 ;
    hl_tetra[G][G][A][G][A][C] = -300 ;
    hl_tetra[C][G][C][A][A][G] = -300 ;
    hl_tetra[G][G][A][A][A][C] = -300 ;
    hl_tetra[C][G][G][A][A][G] = -300 ;
    hl_tetra[C][U][U][C][G][G] = -300 ;
    hl_tetra[C][G][U][G][A][G] = -300 ;
    hl_tetra[C][G][A][A][G][G] = -250 ;
    hl_tetra[C][U][A][C][G][G] = -250 ;
    hl_tetra[G][G][C][A][A][C] = -250 ;
    hl_tetra[C][G][C][G][A][G] = -250 ;
    hl_tetra[U][G][A][G][A][G] = -250 ;
    hl_tetra[C][G][A][G][A][G] = -200 ;
    hl_tetra[A][G][A][A][A][U] = -200 ;
    hl_tetra[C][G][U][A][A][G] = -200 ;
    hl_tetra[C][U][A][A][C][G] = -200 ;
    hl_tetra[U][G][A][A][A][G] = -200 ;
    hl_tetra[G][G][A][A][G][C] = -150 ;
    hl_tetra[G][G][G][A][A][C] = -150 ;
    hl_tetra[U][G][A][A][A][A] = -150 ;
    hl_tetra[A][G][C][A][A][U] = -150 ;
    hl_tetra[A][G][U][A][A][U] = -150 ;
    hl_tetra[C][G][G][G][A][G] = -150 ;
    hl_tetra[A][G][U][G][A][U] = -150 ;
    hl_tetra[G][G][C][G][A][C] = -150 ;
    hl_tetra[G][G][G][A][G][C] = -150 ;
    hl_tetra[G][U][G][A][A][C] = -150 ;
    hl_tetra[U][G][G][A][A][A] = -150 ;
  }

  static int termaupenalty_ar[][] = new int[ASIZE][ASIZE];

  static void init_termaupenalty_ar()
  {
    int i,j;
    for(i=0;i<ASIZE;i++) for(j=0;j<ASIZE;j++) termaupenalty_ar[i][j] = 0;

    termaupenalty_ar[A][U] = 50 ;
    termaupenalty_ar[U][A] = 50 ;
    termaupenalty_ar[G][U] = 50 ;
    termaupenalty_ar[U][G] = 50 ;
  }

  public static int hl_energy(int i, int j)
  {
    int size;
    int entropy;
    int tetra_bonus, stack_mismatch;
    int termaupen;

    size           = j-i-1;
    entropy        = hl_ent(size);
    stack_mismatch = hl_stack(i,j);
    // XXX Hacky, hacky - real fix: fix the index computation in adpc
    if (i+5 >= s.length)
      tetra_bonus = UNDEF;
    else
      tetra_bonus    = hl_tetra[s[i]][s[i+1]][s[i+2]][s[i+3]][s[i+4]][s[i+5]];
    termaupen      = termaupenalty_ar[s[i]][s[j]];

    if (size==3) return(entropy + termaupen);
    if (size==4) return(entropy + stack_mismatch + tetra_bonus);
    if (size>4)  return(entropy + stack_mismatch);

    System.err.println("hairpin loop < 3 found. Please use production");
    System.err.println("  hl <<< lbase -~~ (region `with` minsize 3)  ~~- lbase");
    System.err.println("in your grammar.");
    System.exit(1);
    return -1;
  }

  static int bl_ent_ar[];

  static void init_bl_ent_ar()
  {
    int i;
    bl_ent_ar = new int[ Math.max(31, n+1) ];

    /* report errors by unrealistic values: */
    bl_ent_ar[0] = UNDEF;

    bl_ent_ar[1] = 380 ;
    bl_ent_ar[2] = 280 ;
    bl_ent_ar[3] = 320 ;
    bl_ent_ar[4] = 360 ;
    bl_ent_ar[5] = 400 ;
    bl_ent_ar[6] = 440 ;
    bl_ent_ar[7] = 459 ;
    bl_ent_ar[8] = 470 ;
    bl_ent_ar[9] = 480 ;
    bl_ent_ar[10] = 490 ;
    bl_ent_ar[11] = 500 ;
    bl_ent_ar[12] = 510 ;
    bl_ent_ar[13] = 519 ;
    bl_ent_ar[14] = 527 ;
    bl_ent_ar[15] = 534 ;
    bl_ent_ar[16] = 541 ;
    bl_ent_ar[17] = 548 ;
    bl_ent_ar[18] = 554 ;
    bl_ent_ar[19] = 560 ;
    bl_ent_ar[20] = 565 ;
    bl_ent_ar[21] = 571 ;
    bl_ent_ar[22] = 576 ;
    bl_ent_ar[23] = 580 ;
    bl_ent_ar[24] = 585 ;
    bl_ent_ar[25] = 589 ;
    bl_ent_ar[26] = 594 ;
    bl_ent_ar[27] = 598 ;
    bl_ent_ar[28] = 602 ;
    bl_ent_ar[29] = 605 ;
    bl_ent_ar[30] = 609 ;

    for (i=31;i<=n;i++) bl_ent_ar[i] = bl_ent_ar[30] + jacobson_stockmayer(i);
  }
  
  static int bl_ent(int size) {
    return bl_ent_ar[size];
  }


  /* subword length */
  static int size_of(int i, int j)
  {
    return j-i;
  }

/* Bulge Loop Left                */
/* ------------------------------ */
/*
      											  .        .
      											  .        .
										       (bl+3) - (br-2)   
 If size == 1 the terminal aupenalty for the stem starting after the bulge (that is    (bl+2) - (br-1)) 
										    bl+1
										          bl  -   br			   

 is added possibly. This is unwanted. Since we do not have a chance to check the size of the bulge when parsing the stem
 we substract the possible penalty here! 
*/

  public static int bl_energy(int bl, int i, int j, int br)
  {
    int stacking, size, entropy;

    if (br < 2 || bl < 1)
      return UNDEF;

    stacking    = stack_dg[s[bl]][s[j+1]][s[br-1]][s[br]];
    size        = size_of(i,j);
    entropy     = bl_ent(size);

    if     (size==1)  return(stacking + entropy - termaupenalty_ar[s[bl+2]][s[br-1]]);
    else if (size>1)  return(entropy + termaupenalty_ar[s[bl]][s[br]]);
    else {
      System.err.println("bl_energy size < 1"); System.exit(1); return -1;}
  }

/* Bulge Loop Right               */
/* ------------------------------ */

  public static int br_energy(int bl, int i, int j, int br)
  {
    int stacking, size, entropy;

    // XXX
    if (bl<1)
      return UNDEF;

    stacking    = stack_dg[s[bl]][s[bl+1]][s[i]][s[br]];
    size        = size_of(i,j);
    entropy     = bl_ent(size);

    if (size==1) return(stacking + entropy - termaupenalty_ar[s[bl+1]][s[br-2]]);
    if (size>1)  return(entropy + termaupenalty_ar[s[bl]][s[br]]);

    System.err.println("br_energy size < 1");
    System.exit(1);
    return -1;
  }

  public static int termaupenalty(int i, int j)
  {
    if (i<1)
      return UNDEF;
    return termaupenalty_ar[s[i]][s[j]];
  }

/* Entropic Term                  */
/* ------------------------------ */
/*
  DESTABILIZING ENERGIES BY SIZE OF LOOP

  il_ent 1 and 2 undefined in the tables of Mathews et al. since
  special energy values exist
*/

  static int[] il_ent_ar;

  static void init_il_ent_ar()
  {
    int i;
    il_ent_ar = new int [Math.max(31, n+1)];

    /* report errors by unrealistic values: */
    il_ent_ar[ 0] = UNDEF;
    il_ent_ar[ 1] = UNDEF;

    il_ent_ar[2] = 150  ;
    il_ent_ar[3] = 160 ;
    il_ent_ar[4] = 170 ;
    il_ent_ar[5] = 180 ;
    il_ent_ar[6] = 200 ;
    il_ent_ar[7] = 220 ;
    il_ent_ar[8] = 230 ;
    il_ent_ar[9] = 240 ;
    il_ent_ar[10] = 250 ;
    il_ent_ar[11] = 260 ;
    il_ent_ar[12] = 270 ;
    il_ent_ar[13] = 278 ;
    il_ent_ar[14] = 286 ;
    il_ent_ar[15] = 294 ;
    il_ent_ar[16] = 301 ;
    il_ent_ar[17] = 307 ;
    il_ent_ar[18] = 313 ;
    il_ent_ar[19] = 319 ;
    il_ent_ar[20] = 325 ;
    il_ent_ar[21] = 330 ;
    il_ent_ar[22] = 335 ;
    il_ent_ar[23] = 340 ;
    il_ent_ar[24] = 345 ;
    il_ent_ar[25] = 349 ;
    il_ent_ar[26] = 353 ;
    il_ent_ar[27] = 357 ;
    il_ent_ar[28] = 361 ;
    il_ent_ar[29] = 365 ;
    il_ent_ar[30] = 369 ;

    for (i=31;i<=n;i++) il_ent_ar[i] = il_ent_ar[30] + jacobson_stockmayer(i);
  }

  static int il_ent(int size)
  {
    return il_ent_ar[size];
  }


  static int[][][][] tstacki_dg = new int[ASIZE][ASIZE][ASIZE][ASIZE];

/* Stacking Interaction           */
/* ------------------------------ */
/*

STACKING ENERGIES : TERMINAL MISMATCHES AND BASE-PAIRS.

Stabilizing energies for canonical basepairs: AU, CG, GU
Basepairing: Paramers are in 5' 3' order.
tstacki_dg a b c d
           ^ ^ ^ ^
           | |_| |
           |_____|

*/

  static void init_tstacki_dg()
  {
    int i,j,k,l;

    /* set to maximal value: */
    for(i=0;i<ASIZE;i++) for(j=0;j<ASIZE;j++) for(k=0;k<ASIZE;k++) for(l=0;l<ASIZE;l++) tstacki_dg[i][j][k][l] = 0;

    tstacki_dg[C][A][A][G] =    0 ;
    tstacki_dg[C][A][C][G] =    0 ;
    tstacki_dg[C][A][G][G] = -110 ;
    tstacki_dg[C][A][U][G] =    0 ;
    tstacki_dg[C][C][A][G] =    0 ;
    tstacki_dg[C][C][C][G] =    0 ;
    tstacki_dg[C][C][G][G] =    0 ;
    tstacki_dg[C][C][U][G] =    0 ;
    tstacki_dg[C][G][A][G] = -110 ;
    tstacki_dg[C][G][C][G] =    0 ;
    tstacki_dg[C][G][G][G] =    0 ;
    tstacki_dg[C][G][U][G] =    0 ;
    tstacki_dg[C][U][A][G] =    0 ;
    tstacki_dg[C][U][C][G] =    0 ;
    tstacki_dg[C][U][G][G] =    0 ;
    tstacki_dg[C][U][U][G] =  -70 ;
    tstacki_dg[G][A][A][C] =    0 ;
    tstacki_dg[G][A][C][C] =    0 ;
    tstacki_dg[G][A][G][C] = -110 ;
    tstacki_dg[G][A][U][C] =    0 ;
    tstacki_dg[G][C][A][C] =    0 ;
    tstacki_dg[G][C][C][C] =    0 ;
    tstacki_dg[G][C][G][C] =    0 ;
    tstacki_dg[G][C][U][C] =    0 ;
    tstacki_dg[G][G][A][C] = -110 ;
    tstacki_dg[G][G][C][C] =    0 ;
    tstacki_dg[G][G][G][C] =    0 ;
    tstacki_dg[G][G][U][C] =    0 ;
    tstacki_dg[G][U][A][C] =    0 ;
    tstacki_dg[G][U][C][C] =    0 ;
    tstacki_dg[G][U][G][C] =    0 ;
    tstacki_dg[G][U][U][C] =  -70 ;
    tstacki_dg[G][A][A][U] =   70 ;
    tstacki_dg[G][A][C][U] =   70 ;
    tstacki_dg[G][A][G][U] =  -40 ;
    tstacki_dg[G][A][U][U] =   70 ;
    tstacki_dg[G][C][A][U] =   70 ;
    tstacki_dg[G][C][C][U] =   70 ;
    tstacki_dg[G][C][G][U] =   70 ;
    tstacki_dg[G][C][U][U] =   70 ;
    tstacki_dg[G][G][A][U] =  -40 ;
    tstacki_dg[G][G][C][U] =   70 ;
    tstacki_dg[G][G][G][U] =   70 ;
    tstacki_dg[G][G][U][U] =   70 ;
    tstacki_dg[G][U][A][U] =   70 ;
    tstacki_dg[G][U][C][U] =   70 ;
    tstacki_dg[G][U][G][U] =   70 ;
    tstacki_dg[G][U][U][U] =    0 ;
    tstacki_dg[U][A][A][G] =   70 ;
    tstacki_dg[U][A][C][G] =   70 ;
    tstacki_dg[U][A][G][G] =  -40 ;
    tstacki_dg[U][A][U][G] =   70 ;
    tstacki_dg[U][C][A][G] =   70 ;
    tstacki_dg[U][C][C][G] =   70 ;
    tstacki_dg[U][C][G][G] =   70 ;
    tstacki_dg[U][C][U][G] =   70 ;
    tstacki_dg[U][G][A][G] =  -40 ;
    tstacki_dg[U][G][C][G] =   70 ;
    tstacki_dg[U][G][G][G] =   70 ;
    tstacki_dg[U][G][U][G] =   70 ;
    tstacki_dg[U][U][A][G] =   70 ;
    tstacki_dg[U][U][C][G] =   70 ;
    tstacki_dg[U][U][G][G] =   70 ;
    tstacki_dg[U][U][U][G] =    0 ;
    tstacki_dg[A][A][A][U] =   70 ;
    tstacki_dg[A][A][C][U] =   70 ;
    tstacki_dg[A][A][G][U] =  -40 ;
    tstacki_dg[A][A][U][U] =   70 ;
    tstacki_dg[A][C][A][U] =   70 ;
    tstacki_dg[A][C][C][U] =   70 ;
    tstacki_dg[A][C][G][U] =   70 ;
    tstacki_dg[A][C][U][U] =   70 ;
    tstacki_dg[A][G][A][U] =  -40 ;
    tstacki_dg[A][G][C][U] =   70 ;
    tstacki_dg[A][G][G][U] =   70 ;
    tstacki_dg[A][G][U][U] =   70 ;
    tstacki_dg[A][U][A][U] =   70 ;
    tstacki_dg[A][U][C][U] =   70 ;
    tstacki_dg[A][U][G][U] =   70 ;
    tstacki_dg[A][U][U][U] =    0 ;
    tstacki_dg[U][A][A][A] =   70 ;
    tstacki_dg[U][A][C][A] =   70 ;
    tstacki_dg[U][A][G][A] =  -40 ;
    tstacki_dg[U][A][U][A] =   70 ;
    tstacki_dg[U][C][A][A] =   70 ;
    tstacki_dg[U][C][C][A] =   70 ;
    tstacki_dg[U][C][G][A] =   70 ;
    tstacki_dg[U][C][U][A] =   70 ;
    tstacki_dg[U][G][A][A] =  -40 ;
    tstacki_dg[U][G][C][A] =   70 ;
    tstacki_dg[U][G][G][A] =   70 ;
    tstacki_dg[U][G][U][A] =   70 ;
    tstacki_dg[U][U][A][A] =   70 ;
    tstacki_dg[U][U][C][A] =   70 ;
    tstacki_dg[U][U][G][A] =   70 ;
    tstacki_dg[U][U][U][A] =    0 ;
  }

  static int il_stack(int i, int j, int k, int l)
  {
    return tstacki_dg[s[i]][s[i+1]][s[l]][s[l+1]] 
         + tstacki_dg[s[j+1]][s[j]][s[k+1]][s[k]];
  }

  /* Ninio's equation */
  static int il_asym(int SL, int SR) 
  {
    return Math.min(300,((Math.abs((SL)-(SR)))*50));
  }

  public static int il_energy(int i, int j, int k, int l)
  {
    int sl, sr;
    sl = size_of(i,j);
    sr = size_of(k,l);
    if ((sl > 2) || (sr > 2)) 
      return((il_ent (sl + sr))
          + (il_stack (i,j,k,l))
          + (il_asym(sl,sr))); else
        if ((sl == 1) && (sr == 1)) return(il11_energy(i,l+1)); else  
          if ((sl == 1) && (sr == 2)) return(il12_energy(i,l+1)); else  
            if ((sl == 2) && (sr == 1)) return(il21_energy(i,l+1)); else  
              if ((sl == 2) && (sr == 2)) return(il22_energy(i,l+1)); else
                return 65000;
  }

  static int dr_dangle_dg[][][] = new int [ASIZE][ASIZE][ASIZE+1];

/* dangle right                   */
/* ------------------------------ */

  static void init_dr_dangle_dg()
  {
    int i,j,k;
    /* report errors by unrealistic values: */
    for(i=0;i<ASIZE;i++) for(j=0;j<ASIZE;j++) for(k=0;k<ASIZE+1;k++) dr_dangle_dg[i][j][k] = 0;

    dr_dangle_dg[C][G][A] = -110 ;
    dr_dangle_dg[C][G][C] =  -40 ;
    dr_dangle_dg[C][G][G] = -130 ;
    dr_dangle_dg[C][G][U] =  -60 ;

    dr_dangle_dg[G][C][A] = -170 ;
    dr_dangle_dg[G][C][C] =  -80 ;
    dr_dangle_dg[G][C][G] = -170 ;
    dr_dangle_dg[G][C][U] = -120 ;

    dr_dangle_dg[G][U][A] =  -70 ;
    dr_dangle_dg[G][U][C] =  -10 ;
    dr_dangle_dg[G][U][G] =  -70 ;
    dr_dangle_dg[G][U][U] =  -10 ;

    dr_dangle_dg[U][G][A] =  -80 ;
    dr_dangle_dg[U][G][C] =  -50 ;
    dr_dangle_dg[U][G][G] =  -80 ;
    dr_dangle_dg[U][G][U] =  -60 ;

    dr_dangle_dg[A][U][A] =  -70 ;
    dr_dangle_dg[A][U][C] =  -10 ;
    dr_dangle_dg[A][U][G] =  -70 ;
    dr_dangle_dg[A][U][U] =  -10 ;

    dr_dangle_dg[U][A][A] =  -80 ;
    dr_dangle_dg[U][A][C] =  -50 ;
    dr_dangle_dg[U][A][G] =  -80 ;
    dr_dangle_dg[U][A][U] =  -60 ;
  }

  public static int dli_energy(int i, int j)
  {
    return dr_dangle_dg[s[j]][s[i]][s[i+1]];
  }

  public static int dr_energy(int i, int j)
  {
    // XXX real fix: adpc index computation
    if (i<1 || j+1 >= s.length)
      return UNDEF;
    else
      return dr_dangle_dg[s[i]][s[j]][s[j+1]];
  }

  static int dl_dangle_dg [][][] = new int [ASIZE+1][ASIZE][ASIZE];

  /* dangle left                    */
  /* ------------------------------ */

  static void init_dl_dangle_dg()
  {
    int i,j,k;
    /* report errors by unrealistic values: */
    for(i=0;i<ASIZE+1;i++) for(j=0;j<ASIZE;j++) for(k=0;k<ASIZE;k++) dl_dangle_dg[i][j][k] = 0;

    dl_dangle_dg[A][C][G] =  -50 ;
    dl_dangle_dg[C][C][G] =  -30 ;
    dl_dangle_dg[G][C][G] =  -20 ;
    dl_dangle_dg[U][C][G] =  -10 ;

    dl_dangle_dg[A][G][C] =  -20 ;
    dl_dangle_dg[C][G][C] =  -30 ;
    dl_dangle_dg[G][G][C] =    0 ;
    dl_dangle_dg[U][G][C] =    0 ;

    dl_dangle_dg[A][G][U] =  -30 ;
    dl_dangle_dg[C][G][U] =  -30 ;
    dl_dangle_dg[G][G][U] =  -40 ;
    dl_dangle_dg[U][G][U] =  -20 ;

    dl_dangle_dg[A][U][G] =  -30 ;
    dl_dangle_dg[C][U][G] =  -10 ;
    dl_dangle_dg[G][U][G] =  -20 ;
    dl_dangle_dg[U][U][G] =  -20 ;

    dl_dangle_dg[A][A][U] =  -30 ;
    dl_dangle_dg[C][A][U] =  -30 ;
    dl_dangle_dg[G][A][U] =  -40 ;
    dl_dangle_dg[U][A][U] =  -20 ;

    dl_dangle_dg[A][U][A] =  -30 ;
    dl_dangle_dg[C][U][A] =  -10 ;
    dl_dangle_dg[G][U][A] =  -20 ;
    dl_dangle_dg[U][U][A] =  -20 ;
  }

  public static int dri_energy(int i, int j)
  {
    return dl_dangle_dg[s[j-1]][s[j]][s[i]];
  }

  public static int dl_energy(int i, int j)
  {
    // XXX real fix in adpc?
    if (i < 2)
      return UNDEF;
    else
      return dl_dangle_dg[s[i-1]][s[i]][s[j]];
  }

  public static int ss_energy(int i, int j) 
  {
    return 0;
  }

  // Some constants for pknotsRG
  // The weighting parameter for pseudoknots 
  public static final int wkn = 1;
  // Unpaired base in a pseudoknot 
  public static final int npp = 30; 
  // Basepair in a pseudoknot 
  public static final int pbp = 0;
  // pseudoknot - init - penalty 
  public static final int pkinit = 900;

  public static void init_rna(byte[] seq)
  {
    s = seq;
    n = s.length-1;
    init_canPair();
    init_stack_dg();
    init_hl_ent_ar();
    init_tstackh_dg();
    init_hl_tetra();
    init_termaupenalty_ar();
    init_bl_ent_ar();
    init_intloop11(s);
    init_intloop21(s);
    init_intloop22(s);
    init_il_ent_ar();
    init_tstacki_dg();
    init_dr_dangle_dg();
    init_dl_dangle_dg();
  }

}
