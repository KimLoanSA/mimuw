#!/bin/bash

( cd ../solve || exit; make clean )
( cd ../solve || exit; make )
( cd ../solve/src/test || exit; make clean )
( cd ../solve/src/test || exit; make )
