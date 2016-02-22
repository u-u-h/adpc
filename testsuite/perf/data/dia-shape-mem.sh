# Called:
#python perf2plot.py -pname=shape,shape2,shape3.y=mem  -l > perf/data/dia-shape-mem-rss.dat
#python perf2plot.py -pname=shape,shape2,shape3.y=vsize  -l > perf/data/dia-shape-mem-vsize.dat

set title "Speicherbedarf von RNAshapes"
set xlabel "n"
set ylabel "mem (kb)"


plot "dia-shape-mem-vsize.dat" index 0 title "vsize (Java)"


set terminal postscript eps #color
set output "dia-shape-mem.eps"

replot \
  "dia-shape-mem-rss.dat" index 0 title "rss (Java)",\
  "dia-shape-mem-vsize.dat" index 1 title "vsize (C)",\
  "dia-shape-mem-rss.dat" index 1 title "rss (C)"
