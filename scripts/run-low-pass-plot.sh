#!/usr/bin/env bash

set -exu

# Make a .wav file, filter it based on its FFT, and then plot using gnuplot

#********

# Basename for generated data files with various extensions
base="build/low-pass-data-1"

# Filtered version
basef="$base"f

fpm run -- "$base".wav --noise 300 10.0
fpm run -- "$base".wav "$base"f.wav --low-pass 1000.0

## Running a low-pass on a square wave yields the expected Fourier components
#fpm run -- "$base".wav --square 300 5
#fpm run -- "$base".wav "$base"f.wav --low-pass 3000.0

fpm run -- --fft "$basef".wav "$basef".csv

## `pause` hangs the bash script here
#gnuplot -e '
#	plot "'"$basef"'.csv" using 1:2 title "real" with linespoints,
#	     "'"$basef"'.csv" using 1:3 title "imag" with linespoints;
#	pause -1;
#'

# Save a png without making any window
gnuplot -e '
	set terminal png size 800,800; 
	set xrange [0: 1200];
	set output "'"$basef"'.png";
	plot "'"$basef"'.csv" using 1:2 title "real" with linespoints,
	     "'"$basef"'.csv" using 1:3 title "imag" with linespoints;
'

