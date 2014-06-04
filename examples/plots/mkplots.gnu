set terminal png enhanced size 600,400 

#unset key
set key top left
#set key outside bottom center horizontal
set ytics nomirror
set xtics nomirror

set xlabel "value of p"
set ylabel "running time (seconds)"
set border 3

set style data histogram
set style histogram cluster gap 1
set style fill solid border -1

set output "supercomp-lebesgue.png" 
set title "run times of L_p nearest neighbor calculation\n(solid columns compiled with fast-math, hashed columns without)"
#set title "run times of L_p nearest neighbor calculation"
plot 'supercomp-lebesgue.dat' using 2:xtic(1) lt 1 lw 1 lc rgb '#007700' title col,\
     'supercomp-lebesgue.dat' using 5:xtic(1) lt 1 lw 1 lc rgb '#007700' title col fillstyle pattern 6,\
     'supercomp-lebesgue.dat' using 3:xtic(1) lt 1 lw 1 lc rgb '#770000' title col,\
     'supercomp-lebesgue.dat' using 6:xtic(1) lt 1 lw 1 lc rgb '#770000' title col fillstyle pattern 6,\
     'supercomp-lebesgue.dat' using 4:xtic(1) lt 1 lw 1 lc rgb '#000077' title col,\
     'supercomp-lebesgue.dat' using 7:xtic(1) lt 1 lw 1 lc rgb '#000077' title col fillstyle pattern 6


set xlabel "number of datapoints (i.e. size of outer vector)"
set style lines 1
#set log x
#set log y
set output "unboxed_vs_boxed.png" 
set title "computing the nearest neighbor of every data point"
plot 'unboxed.dat' using 2:3 lt 1 lw 2 lc rgb '#007700' title col with lines,\
     'unboxed.dat' using 2:4 lt 1 lw 2 lc rgb '#770000' title col with lines
