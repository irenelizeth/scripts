#!/bin/bash

if [ -z "$1" ]; then
	echo first arg is empty 
	echo usage: $0 directory matching_string num_sites
	exit
fi

if [ -z "$2" ]; then 
        echo second arg is empty 
        echo usage: $0 directory matching_string num_sites
        exit
fi

if [ -z "$3" ]; then 
        echo third arg is empty 
        echo usage: $0 directory matching_string num_sites
        exit
fi

DIRECTORY=$1
MATCH_STRING=$2
TSITES=$3

if [ -z "$4" ]; then
	APP_NAME=$MATCH_STRING
else
	APP_NAME=$4
fi

if [ -z "$5" ]; then
	ITER=false
else
	ITER=true
fi

function calculate_total_persite() {
	
	for ((i=0; i<=$TSITES; i++))
	do
        	RESULT=$(ls $1$MATCH_STRING$i*)
        	#echo result value is $RESULT
        	if  [ "$RESULT" > "0" ]; then
                	echo site $i : $(ls $1$MATCH_STRING$i-* | wc -l) >> results-$APP_NAME.txt
        	fi
	done
}

if [ "$ITER" = "true" ]; then

	echo total directories in $DIRECTORY : $(ls -1 $DIRECTORY | wc -l) >> results-$APP_NAME.txt

	PDIR=$DIRECTORY # parent directory
	LIST=$(ls -1 $DIRECTORY)
	echo list = $LIST
	
	for f in $LIST
	do
		#echo $f
		echo total files in $PDIR$f"/" : $(ls -1 $PDIR$f"/" | wc -l) >> results-$APP_NAME.txt
		calculate_total_persite $PDIR$f"/"
		
	done		
else
	echo total files in $DIRECTORY : $(ls -1 $DIRECTORY | wc -l) >> results-$APP_NAME.txt
        calculate_total_persite $DIRECTORY
fi

exit 0
