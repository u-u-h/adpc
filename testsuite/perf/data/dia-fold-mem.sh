# Called:
# python perf2plot.py -pname=fold.y=mem  -l > perf/data/dia-fold-mem-rss.dat
# python perf2plot.py -pname=fold.y=vsize  -l > perf/data/dia-fold-mem-vsize.dat

set title "Speicherbedarf von RNAfold"
set xlabel "n"
set ylabel "mem (kb)"


plot "dia-fold-mem-vsize.dat" index 0 title "vsize (Java)"


set terminal postscript eps #color
set output "dia-fold-mem.eps"

replot \
  "dia-fold-mem-rss.dat" index 0 title "rss (Java)",\
  "dia-fold-mem-vsize.dat" index 1 title "vsize (C)",\
  "dia-fold-mem-rss.dat" index 1 title "rss (C)"
