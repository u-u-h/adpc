# Called:
# python perf2plot.py -i perf/out2 -pname=tdmwin -l > perf/data/dia-tdm-lang.dat

set title "TDM Laufzeit"
set xlabel "n"
set ylabel "utime (s)"
set yrange [] writeback
plot "dia-tdm-lang.dat" index 0 title "Java"
set yrange restore

set terminal postscript eps #color
set output "dia-tdm-lang.eps"

replot \
  "dia-tdm-lang.dat" index 1 title "C"
