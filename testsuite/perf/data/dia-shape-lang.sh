# Called:
# python perf2plot.py -pname=shape,shape2,shape3.y=utime -l > perf/data/dia-shape-lang.dat

set title "RNAshapes Laufzeit"
set xlabel "n"
set ylabel "utime (s)"
set yrange [] writeback
plot "dia-shape-lang.dat" index 0 title "Java"
set yrange restore

set terminal postscript eps #color
set output "dia-shape-lang.eps"

replot \
  "dia-shape-lang.dat" index 1 title "C"
