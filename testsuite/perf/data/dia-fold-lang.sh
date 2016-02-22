# Called:
# python perf2plot.py -pname=fold.y=utime -l > perf/data/dia-fold-lang.dat

set title "RNAfold Laufzeit"
set xlabel "n"
set ylabel "utime (s)"
set yrange [] writeback
plot "dia-fold-lang.dat" index 0 title "Java"
set yrange restore

set terminal postscript eps #color
set output "dia-fold-lang.eps"

replot \
  "dia-fold-lang.dat" index 1 title "C"
