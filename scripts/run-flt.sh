#!/usr/bin/env bash

set -exu

adsr="--adsr 0 0 1 0"
cutoff="--2pole 200"
fadsr="--fadsr 0 4 0 0"

# Frequency and length
f=300
l=5

time fpm run -- tri.wav --tri $f $l $adsr $cutoff $fadsr
time fpm run -- saw.wav --saw $f $l $adsr $cutoff $fadsr
time fpm run -- squ.wav --squ $f $l $adsr $cutoff $fadsr

