#!/bin/bash

if [ -z "$1" ]; then
	echo first arg is empty 
	echo usage: $0 directory matching_string num_sites app_name inter
	exit
fi

if [ -z "$2" ]; then 
        echo second arg is empty 
        echo usage: $0 directory matching_string num_sites app_name inter
        exit
fi

if [ -z "$3" ]; then 
        echo third arg is empty 
        echo usage: $0 directory matching_string num_sites app_name inter
        exit
fi

DIRECTORY=$1
MATCH_STRING=$2
TSITES=$3

# arg $4 is used to indicate name of app subject
if [ -z "$4" ]; then
	APP_NAME=$MATCH_STRING
else
	APP_NAME=$4
fi

# arg $5 is used to indicate if script should be interactive and analyze subdirectories
if [ -z "$5" ]; then
	ITER=false
else
	ITER=true # any value for $5 indicates it should be interactive
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
	#TO-DO: test when non-interactive
        calculate_total_persite $DIRECTORY
fi

exit 0
