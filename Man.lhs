> module Man where

> showLongHelp manopt = case manopt of 
>  "h" ->  "-h Display this information\n\nThis option shows the adpcompile command line interface.\n"
 
>  "H" ->  "-H <option> Display detailed information on <option>\n\nThis displays the corresponding section of the adpcompile manual for the\ngiven command line option.\n"
 
>  "c" ->  "-c <file> Compile to imperative target code\n\nCompile the given adp file to C.\n"
 
>  "z" ->  "-z <file> Optimize combinators in ADP source code\n\nThis mode reads the given ADP file, optimizes the next-combinators in\nit, and generates an optimized copy of the given file.\n"
 
>  "l" ->  "-l <file> Generate recurrences typeset in LaTeX\n\nThis option generates recurrences typeset in LaTeX.\n"
 
>  "g" ->  "-g <file> Generate ADP template for signature\n\nThis option generates a ADP template for the given signature. The\ntemplate consists of the algebra type, enumeration and count algebras,\nthe definition of the pair-operator and a simple grammar, that generates\nthe term algebra.\n"
 
>  "tg" ->  "-tg <file> -ctg Derive good table configurations\n\nOption -tg generates a <good> table configuration. A configuration is\ngood, if it achieves a polynomial runtime of the given ADP program with\nthe minimal number of DP tables. While option -tg stops after the table\ndesign, the option -ctg can be used together with option -c for the\nimperative target code generation.\n"
 
>  "to" ->  "-to <file> -cto Derive optimal table configurations\n\nOption -to generates an optimal table configuration. A configuration is\noptimal, if it achieves the best possible asymptotic runtime with the\nminimal number of DP tables. Again, option -to starts a stand-alone\ntable design, while -cto optimizes the tables during the target code\ngeneration.\n"
 
>  "tx" ->  "-tx <file> -ctx Derive approx. optimal table configurations\n\nSince the table design is an NP-complete problem, options -tg and -to\nare not suitable for larger grammars. Our experience showed that\ngrammars with up to 40 nonterminal symbols can be processed with option\n-to. With more nonterminals, the running time of the optimization can\neasily go into hours and days. Here the options -tx and -ctx can be\nused. These options calculate optimal table configurations with the use\nof an approximal approach (GRASP). In our experience this approximal\nresults are as good as the results from option -to.\n"
 
>  "iuc" ->  "-iuc Ignore user annotated table configuration\n\nTo support the table design phase, the input program can be annotated\nwith the keywords \"tabulated\" and \"nontabulated\". This means, a\nproduction annotated with \"tabulated\" has to be tabulated in every case,\nand a production annotated with \"nontabulated\" must not be tabulated.\nThis user annotations work together with large grammars and options -tg\nand -to, which would otherwise calculate too long. With option -iuc the\nADP compiler ignores all user annotations.\n"
 
>  "tadd" ->  "-tadd <number> Maximal number of additional tables\n\nOption -tadd can be used together with options -to and -tx to improve\nthe constant factor of an optimal table configuration. This is done by\nintroducing a given number of additional tables. If this improves\nconstant factors by the factor given with option -taddc the additional\ntables are added to the table configuration.\n"
 
>  "taddc" ->  "-taddc <number> Necessary constant factor improvement for add. tables\n\nOption -taddc specifies the necessary constant factor improvement for\noption -tadd\n"
 
>  "taddn" ->  "-taddn <number> Expected input length for constant factor improvement\n\nOption -taddn specifies the expected input length for the constant\nfactor improvement.\n"
 
>  "bt" ->  "-bt <mode> Generate backtracing code (enumeration algebra needed) mode:\ns -> single, so -> suboptimal\n\nWith option -bt the ADP compiler generates backtracing code. The given\nmode can be either <s> for a single-result-backtrace, or <so> for a\nsuboptimal backtrace.\n"
 
>  "W" ->  "-W Include window mode\n\nOption -W generates program code for a sliding window mode. Beginning\nwith position 1 of the input sequence, the analysis is repeatedly\nprocessed on subsequences of the specified size. After each calculation,\nthe results are printed out and the window is moved by the window\nposition increment (-W), until the end of the input sequence is reached.\n"
 
>  "al" ->  "-al <alg> .. <alg> Specify order of algebra usage\n\nOption -al specifies the algebras for the compilation.\n"
 
>  "alpp" ->  "-alpp <alg> .. <alg> Specify pretty printing algebras\n\nOption -alpp specifies the pretty printing algebra that shall be used\nfor the candidate ouput in the backtrace modes.\n"
 
>  "cs" ->  "-cs <alg> Automatically generate signature and enumeration for algebra\n<alg>\n\nThe option -cs automatically generates the signature and enumeration\nalgebra from the given algebra. This option is useful for the\nbacktracing modes.\n"
 
>  "vl" ->  "-vl <verbosity level> Specify output verbosity level verbosity level: t\n-> target, r -> trace, rr -> detailed trace d -> debug\n\nOption -vl specifies the verbosity level of the compilation.\n"
 
>  "o" ->  "-o <filename> Output to <filename>\n\nOption -o specifes the output file.\n"
 
>  "v" ->  "-v Show version\n\nThis option shows the adpcompile version number.\n\n"
 
>  x -> error $ "unknown command line option -" ++ x
