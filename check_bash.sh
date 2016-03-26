#!/bin/bash

file='run_parSemCo'
ext='.sh'
path='./scripts/'
for i in `seq 1 10`;
do
    	
	# s = "$filename$i$e"
	# echo "$filename"
	# echo "$e"
	echo sbatch $path$file$i$ext
	# echo "$s"

done
