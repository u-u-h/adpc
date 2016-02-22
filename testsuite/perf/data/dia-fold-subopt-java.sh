# Called:
# python perf2plot.py -pname=fold -s > perf/data/dia-fold-subopt-java.dat

set title "RNAfold Laufzeit (Java)"
set xlabel "n"
set ylabel "utime (s)"
set yrange [0:50]
plot "dia-fold-subopt-java.dat" index 0 title "subopt = 0 %"

set terminal postscript eps #color
set output "dia-fold-subopt-java.eps"

set yrange [0:50]
replot \
  "dia-fold-subopt-java.dat" index 1 title "subopt = 5 %", \
  "dia-fold-subopt-java.dat" index 2 title "subopt = 10 %", \
  "dia-fold-subopt-java.dat" index 3 title "subopt = 15 %"
