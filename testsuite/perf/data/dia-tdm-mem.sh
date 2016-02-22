# Called:
# python perf2plot.py -iperf/out2 -pname=tdmwin.y=mem -l > perf/data/dia-tdm-mem-rss.dat
# python perf2plot.py -iperf/out2 -pname=tdmwin.y=vsize -l > perf/data/dia-tdm-mem-vsize.dat


set title "Speicherbedarf von TDM"
set xlabel "n"
set ylabel "mem (kb)"


plot "dia-tdm-mem-vsize.dat" index 0 title "vsize (Java)"


set terminal postscript eps #color
set output "dia-tdm-mem.eps"

replot \
  "dia-tdm-mem-rss.dat" index 0 title "rss (Java)",\
  "dia-tdm-mem-vsize.dat" index 1 title "vsize (C)",\
  "dia-tdm-mem-rss.dat" index 1 title "rss (C)"
