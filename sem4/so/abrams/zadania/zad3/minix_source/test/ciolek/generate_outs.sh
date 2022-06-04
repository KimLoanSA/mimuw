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
    echo "Running ""$test_name""... "

    expected_out="outs/""$test_name"".out" 

    "./bin/""$test_name" 2>&1 | tee "$expected_out"
done

