s/@HEADER@/import static ADPC.RNA.*;\
import ADPC.Options;\
import ADPC.Sequence;/

s/@ALGEBRA_NAME@/mfe/

s/@RNALIB_FREE@//

s/@OUTPUT_OPTIMAL@/\
if (opts.traceback_percent > 0)\
  traceback_diff = Math.abs(result_score * opts.traceback_percent \/ 100);\
print_optimal(result_score);/

s/@OUTPUT_SUBOPT@/print_suboptimal(score, pp_outp);/

s/@OUTPUT_SUBOPT_START@/print_start();/

s/@OUTPUT_SUBOPT_END@/print_end();/

s/@TRAILER@/\
\
Options opts;\
Sequence seq;\
\
public Algebra_mfe(Sequence seq, Options opts)\
{\
  if (opts.window_mode == 1) \
    n = opts.window_size;\
  else\
    n = seq.sequence.length-1;\
  z = seq.sequence;\
  this.seq = seq;\
  this.opts = opts;\
}\
\
public static void main(String[] argv)\
{\
  byte[] seq = convert_input(argv[0]);\
\
  init_rna(seq);\
\
  Sequence sequence = new Sequence();\
  sequence.sequence = seq;\
  sequence.backup = seq.clone();\
  sequence.unconverted = argv[0];\
  sequence.original_length = argv[0].length();\
\
  Options opts = new Options();\
  \/\/opts.traceback_percent = 10;\
  opts.traceback_percent = 0;\
\
  \/\/ zum testen Window mode deaktiviert\
  \/\/opts.window_mode = 0;\
  opts.window_mode = 1;\
  \/\/opts.window_size = argv[0].length();\
  opts.window_size = 20;\
  opts.window_step = 1;\
\
  Algebra_mfe s = new Algebra_mfe(sequence, opts);\
  s.traceback_diff = 0*100;\
\
  s.mainloop();\
  s.freeall();\
  \
}\
/

s/String z;/byte[] z;/
