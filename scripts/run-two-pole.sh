#!/usr/bin/env bash

set -exu

# Generate noise with a two-pole low pass filter and plot its FFT

#********

# Basename for generated data files with various extensions
base="build/two-pole-data-1"

fpm run -- "$base".wav --noise 300 10.0 --two-pole 500.0 --adsr 0 0 1 0

fpm run -- --fft "$base".wav "$base".csv

## `pause` hangs the bash script here
#gnuplot -e '
#	plot "'"$base"'.csv" using 1:2 title "real" with linespoints,
#	     "'"$base"'.csv" using 1:3 title "imag" with linespoints;
#	pause -1;
#'

# Save a png without making any window
gnuplot -e '
	set terminal png size 800,800; 
	set xrange [0: 1200];
	set output "'"$base"'.png";
	plot "'"$base"'.csv" using 1:2 title "real" with linespoints,
	     "'"$base"'.csv" using 1:3 title "imag" with linespoints;
'

