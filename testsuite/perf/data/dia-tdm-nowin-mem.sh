# Called:
# python perf2plot.py -pname=tdmwin.y=vsize -l > perf/data/dia-tdm-nowin-mem-vsize.dat
# python perf2plot.py -pname=tdmwin.y=mem -l > perf/data/dia-tdm-nowin-mem-rss.dat

set title "TDM Speicherbedarf (ohne Window Mode)"
set xlabel "n"
set ylabel "mem (kb)"
set yrange [] writeback
plot "dia-tdm-nowin-mem-vsize.dat" index 1 title "C vsize" with points pointsize 2
set yrange restore

set terminal postscript eps #color
set output "dia-tdm-nowin-mem.eps"

replot \
  "dia-tdm-nowin-mem-vsize.dat" index 0 title "Java vsize" ,\
  "dia-tdm-nowin-mem-rss.dat" index 1 title "C rss" , \
  "dia-tdm-nowin-mem-rss.dat" index 0 title "java rss"
