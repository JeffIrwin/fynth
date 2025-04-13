#!/usr/bin/env bash

set -exu

# Make a sine wave .wav file, convert it to .csv, and then plot using gnuplot.
# Use a low frequency and a short length to avoid making too much data (and to
# help make the plot readable)

suffix="1"

#fpm run -- sin-"$suffix".wav --square 30.0 0.1
fpm run -- sin-"$suffix".wav --sine 30.0 0.1

fpm run -- sin-"$suffix".wav sin-"$suffix".csv

# `pause` hangs the bash script here, but allows interactive zooming.  you can
# alternatively use `gnuplot -p -e ...` to keep the plot window open after
# gnuplot exits, but then you can't zoom!
#
# To zoom in gnuplot, right-click and then left-click to draw the zoom rectangle

#gnuplot -e 'plot "sin-'"$suffix"'.csv" with linespoints ; pause -1'
#gnuplot -p -e 'plot "sin-'"$suffix"'.csv" with linespoints'

# Save a png without making any window
gnuplot -e '
	set terminal png size 800,800; 
	set output "sin-'"$suffix"'.png";
	plot "sin-'"$suffix"'.csv" with linespoints
'

