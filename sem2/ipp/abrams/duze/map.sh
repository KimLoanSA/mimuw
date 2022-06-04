#!/bin/bash
	
	if (($# < 2))
	then
		exit 1
	fi 

	if ! [ -f $1 ] 
	then 
		exit 1
	fi
	
	for i in $@
	do	
		if [ $i == $1 ] 
		then 
			continue
		fi 
		
		re='^[0-9]+$'
		if ! [[ $i =~ $re ]] || (($i <= 0 || $i > 999))
		then
			exit 1
		fi
		
		grep "^$i;" $1 | 
		sed "s/[[:space:]]/_/g" |
		sed "s/\;/ /g" |
		awk '{
		wynik = 0
		for (i = 3; $i; i += 3)
			wynik += $i
		print $1 ";" wynik}'
		
	done

exit 0
