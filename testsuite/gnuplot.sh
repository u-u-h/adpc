#f(x) = a*x**3
#fit f(x) "0.dat" via a
#set yrange [0:100]
#plot  f(x), "0.dat" index 0, "0.dat" index 1, "0.dat" index 2, "0.dat" index 3


#point styles 1, 3, 4, 6, 9, 11, 12, 14, 15, 16

f(x) = a*x**3
#a = 1
fit f(x) "0.dat" via a

set title "Java subopt plot"
set xlabel "n"
set ylabel "utime (s)"
set yrange [] writeback
plot f(x), "0.dat" index 0 with points 12
set yrange restore
replot "0.dat" index 1 with points 14# , "0.dat" index 2 with points 16, "0.dat" index 3
