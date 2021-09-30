TESTS=$(ls src | grep '.c' | sed -e 's/\.c$//')

# Compile stuff, makefiles dont work on minix 0o
mkdir -p bin
for test_name in $TESTS
do
    clang "src/""$test_name"".c" -o "bin/""$test_name"
    if [ $? != 0 ]; then
        exit 1
    fi
done
echo "Compiled succesfully!"
echo ""

# Run tests
for test_name in $TESTS
do
    echo -n "Running ""$test_name""... "

    "./bin/""$test_name" > out.txt 2>&1

    expected_out="outs/""$test_name"".out" 

    diff out.txt "$expected_out"
    if [ $? != 0 ]
    then
        echo "ERROR"
        echo ""
        echo "out:"
        cat out.txt
        echo ""
        echo "expected:"
        cat "$expected_out"
        exit 1
    else
        echo "OK"
    fi
done

