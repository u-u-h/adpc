s/@HEADER@//

s/@ALGEBRA_NAME@/seller/

s/@RNALIB_FREE@//

s/@OUTPUT_OPTIMAL@/print_optimal(result_score);/

s/@OUTPUT_SUBOPT@/print_suboptimal(score, pp_outp);/

s/@OUTPUT_SUBOPT_START@/print_start();/

s/@OUTPUT_SUBOPT_END@/print_end();/

s/@TRAILER@/\
public Algebra_seller(String s)\
{\
  n = s.length();\
  z = " " + s;\
}\
\
public static void main(String[] argv)\
{\
  String seq = argv[0];\
\
  Algebra_seller s = new Algebra_seller(seq);\
  s.traceback_diff = 5;\
  s.mainloop();\
  s.freeall();\
  \
}\
/
