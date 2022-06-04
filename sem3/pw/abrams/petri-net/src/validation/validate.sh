if ! (mkdir $1 && tar xzf $1.tar.gz -C $1)
then
    echo ERROR: unpacking $1
elif ! (cp Validate.java $1 && cd $1 && javac Validate.java)
then
    echo ERROR: compiling Validate
elif ! (cd $1 && java Validate)
then
    echo ERROR: running Validate
fi
j
