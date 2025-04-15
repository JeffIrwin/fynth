#!/usr/bin/env bash

set -exu

# Make a sine wave .wav file, convert it to .csv, and then plot using gnuplot.
# Use a low frequency and a short length to avoid making too much data (and to
# help make the plot readable)

#********

# Basename for generated data files with various extensions
base="build/data-1"

#fpm run -- "$base".wav --square 30.0 0.1
fpm run -- "$base".wav --sine 30.0 0.1

fpm run -- "$base".wav "$base".csv

# `pause` hangs the bash script here, but allows interactive zooming.  you can
# alternatively use `gnuplot -p -e ...` to keep the plot window open after
# gnuplot exits, but then you can't zoom!
#
# To zoom in gnuplot, right-click to draw the zoom rectangle

#gnuplot -e 'plot "'"$base"'.csv" with linespoints ; pause -1'
#gnuplot -p -e 'plot "'"$base"'.csv" with linespoints'

# Save a png without making any window
gnuplot -e '
	set terminal png size 800,800; 
	set output "'"$base"'.png";
	plot "'"$base"'.csv" with linespoints
'

