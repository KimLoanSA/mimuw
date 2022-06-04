for test in ./test*.c; do
    echo "Running test ${test}"
    clang err.c ${test}
    ./a.out
done