#!/bin/bash

# flagi:
# [-c] - czysci wszystko po sobie
# [-b] - tylko buduje
# [-t] - brak kopiowania tesow
# w przeciwnym przypadku buduje i odpala bez czyszczenia

# przetwarzanie flag
run_flag=1
clean_flag=0
copy_test_flag=1

for flag in $@
do
  if [[ "$flag" == "-c" ]]
  then
    clean_flag=1
  fi

  if [[ "$flag" == "-b" ]]
  then
    run_flag=0
  fi

  if [[ "$flag" == "-t" ]]
  then
    copy_test_flag=0
  fi

done


host=students.mimuw.edu.pl

students_root_dir=remote/pix
students_source_dir=${students_root_dir}/src/assembly
students_source_c_dir=${students_root_dir}/src/c
students_test_dir=${students_root_dir}/src/test
students_bash_dir=${students_root_dir}/build/bash
students_target_dir=${students_root_dir}/build/target

## PRZECZYTAJ!

# zeby dzialalo trzeba miec klucze rsa do logowania na studentsa, bez hasla na klucz prywatny

# zmień na swoje!
source_dir=../../src/assembly
source_c_dir=../../src/c
test_dir=../../src/test

# indeks tez!
indeks=ma406058



# budowanie struktury plikow na studentsie
ssh ${indeks}@${host} "mkdir -p ${students_source_c_dir} | mkdir -p ${students_bash_dir} | mkdir -p ${students_target_dir} | mkdir -p ${students_source_dir} | mkdir -p ${students_test_dir}"

# kopiowanie skryptow na studentsa
scp buildAndRun/* ${indeks}@${host}:~/${students_bash_dir}

# kopiowanie kodu
scp ${source_dir}/pix.asm ${indeks}@${host}:${students_source_dir}/pix.asm
scp ${source_c_dir}/pix.h ${indeks}@${host}:${students_source_c_dir}/pix.h
scp ${source_c_dir}/pix_time.c ${indeks}@${host}:${students_source_c_dir}/pix_time.c
scp ${source_c_dir}/pix_test.c ${indeks}@${host}:${students_source_c_dir}/pix_test.c

# kopiowanie testow jesli nie ma flagi [-t]
if [[ ${copy_test_flag} == 1 ]]
then
  scp ${test_dir}/* ${indeks}@students.mimuw.edu.pl:~/remote/src/test
fi

# budowanie rozwiazania
ssh ${indeks}@${host} "./${students_bash_dir}/build.sh ${students_source_dir} ${students_source_c_dir} ${students_target_dir}"

# odpalanie testow jesli nie ma flagi [-b]
if [[ ${run_flag} == 1 ]]
then
  ssh ${indeks}@${host} "./${students_bash_dir}/run_tests.sh ${students_target_dir} ${students_test_dir}"
fi

# czyszczenie jesli jest flaga [-c]
if [[ ${clean_flag} == 1 ]]
then
  ssh ${indeks}@${host} "rm -r -f ${students_root_dir}"
fi
