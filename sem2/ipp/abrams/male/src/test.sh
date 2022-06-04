#!/bin/bash

counter=0
maximum=0

for tempTest in $2/*.in
do
	tempCounter=0;

	valgrind --error-exitcode=15 --leak-check=full --show-leak-kinds=all --errors-for-leak-kinds=all --main-stacksize=999999999 -q ./$1 < $tempTest 1> program.out 2> program.err
	valueOfValgrind=$?

	echo -n -e "$((maximum + 1)):	\e[1mANS: \e[0m"

	if ! diff program.out ${tempTest%in}out >/dev/null 2>&1
	then
		echo -e -n "\e[31;21mWA \e[0m"
	else 
		echo -e -n "\e[32;21mOK \e[0m"
		((tempCounter++))
	fi

	echo -e -n "	\e[1mERR: \e[0m"
	if ! diff program.err ${tempTest%in}err >/dev/null 2>&1
	then
		echo -e -n "\e[31;21mWA \e[0m"
	else 
		echo -e -n "\e[32;21mOK \e[0m"
		((tempCounter++))
	fi

	echo -e -n "	\e[1mMEM: \e[0m"

	if ((valueOfValgrind == 0))
	then
		echo -e -n "\e[32;21mOK \e[0m"
		((tempCounter++))
	else 
		echo -e -n "\e[31;21mRE \e[0m"
	fi

	if (($tempCounter == 3))
	then
		((counter++))
	fi

		((maximum++))

	echo "	for $tempTest"
done

echo "$counter/$maximum tests passed"

rm program.out
rm program.err
