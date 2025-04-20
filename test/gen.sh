#!/usr/bin/env bash

set -eu
#set -exu

# This bash script generates some Fortran source code for MD5 test baselines

for i in {0..70} ; do
#for i in {0..10} ; do

	# Beware repeated magic number 60
	prefix=$(printf 'jeff was here%.0s' {1..60})

	suffix=""
	for ((j=1; j<=i ; j++)); do
		suffix+="."
	done	
	#echo "suffix = $suffix"

	message="$prefix$suffix"
	
	hash=$(echo -n "$message" | md5sum | cut -d " " -f 1)
	
	#echo "hash = $hash"
	
	echo -e "\tnfail = nfail + test_eq(md5_str( &"
	echo -e "\t\trepeat("'"'"jeff was here"'"'", 60) &"
	echo -e "\t\t// repeat("'"'"."'"'", $i)), &"
	echo -e "\t\t"'"'"$hash"'"'", ntot)"

done

