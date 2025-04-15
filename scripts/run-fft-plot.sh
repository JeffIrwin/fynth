#!/usr/bin/env bash

set -exu

# Make a sine wave .wav file, save its FFT to .csv, and then plot using gnuplot.
# Use a low frequency and a short length to avoid making too much data (and to
# help make the plot readable)

#********

# Basename for generated data files with various extensions
base="build/fft-data-1"

fpm run -- "$base".wav --square 30.0 1.0
#fpm run -- "$base".wav --sine 30.0 1.0

fpm run -- --fft "$base".wav "$base".csv

# Square waves have higher harmonics, so plot FFT up to an `xrange` of 220 Hz

# `pause` hangs the bash script here, but allows interactive zooming.  you can
# alternatively use `gnuplot -p -e ...` to keep the plot window open after
# gnuplot exits, but then you can't zoom!
#
# To zoom in gnuplot, right-click to draw the zoom rectangle

#gnuplot -e '
#	set xrange [0: 220];
#	plot "'"$base"'.csv" using 1:2 title "real" with linespoints,
#	     "'"$base"'.csv" using 1:3 title "imag" with linespoints;
#	pause -1;
#'

# Save a png without making any window
gnuplot -e '
	set terminal png size 800,800; 
	set xrange [0: 220];
	set output "'"$base"'.png";
	plot "'"$base"'.csv" using 1:2 title "real" with linespoints,
	     "'"$base"'.csv" using 1:3 title "imag" with linespoints;
'

