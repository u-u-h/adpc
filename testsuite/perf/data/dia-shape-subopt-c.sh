# Called:
# python perf2plot.py -pname=shape,shape2,shape3  -s > perf/data/dia-shape-subopt-c.dat

set title "RNAshape Laufzeit (C)"
set xlabel "n"
set ylabel "utime (s)"
set yrange [0:200]
plot "dia-shape-subopt-c.dat" index 0 title "subopt = 0 %"

set terminal postscript eps #color
set output "dia-shape-subopt-c.eps"

set yrange [0:200]
replot \
  "dia-shape-subopt-c.dat" index 1 title "subopt = 5 %", \
  "dia-shape-subopt-c.dat" index 2 title "subopt = 10 %", \
  "dia-shape-subopt-c.dat" index 3 title "subopt = 15 %"

