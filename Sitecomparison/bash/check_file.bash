#!/bin/bash

# A script to examine the number of commas in each line of a set of csv-file

## Directory containing CSV files
#directory="/path/to/directory"
#
## Loop through each CSV file in the directory
#for file in "$directory"/*.csv; do
#    # Check if the file is a regular file
#    if [ -f "$file" ]; then
#        # Calculate the number of lines and words in the file
#        lines=$(wc -l < "$file")
#        words=$(wc -w < "$file")
#
#        # Calculate the ratio of lines to words
#        ratio=$(awk "BEGIN {print $words / $lines}")
#
#        # Check if the ratio equals 5
#        if [ "$(awk "BEGIN {print ($ratio == 5)}")" -eq 1 ]; then
#            echo "Ratio of lines to words in $file is 5. Keeping the file."
#        else
#            echo "Ratio of lines to words in $file is not 5. Removing the file."
#            rm "$file"
#        fi
#    fi
#done
#
#

#set -ex

thisdir=$1
if [[ "$thisdir" =~ "SODA" ]]; then
	numberMast=1728 
	numberFlux=240
fi

if [[ "$thisdir" =~ "CABA" ]]; then
	numberMast=1728
	numberFlux=720
fi

if [[ "$thisdir" =~ "LIND" ]]; then
	numberMast=1440
	numberFlux=240
fi

for file in ${thisdir}/*Mast*txt; 
do if [ $(wc -w < "$file") != $numberMast ]; then 
       	echo "removing bad file $file"; rm $file
	fi
done

for file in ${thisdir}/*Flux*txt; 
do if [ $(wc -w < "$file") != $numberFlux ]; then 
       	echo "removing bad file $file"; rm $file
	fi
done

#while IFS= read -r line; do echo $line wc; done < xx
