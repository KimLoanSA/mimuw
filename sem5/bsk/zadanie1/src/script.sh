#!/usr/bin/env bash

input_employees=$1
declare -A employees

# clearing dirs
rm -rf kredyty
rm -rf lokaty
rm -rf zadania

# parsing input employees
i=0
while read -r line; do
  employee_info=($line)

  employees[$i, id]=${employee_info[0]}
  employees[$i, name]=${employee_info[1]}
  employees[$i, surname]=${employee_info[2]}
  employees[$i, position]=${employee_info[3]}
  employees[$i, departament]=$(echo "${employee_info[4]}" | tr '[:upper:]' '[:lower:]')

  ((i++))
done < "$input_employees"
number_of_employees=$i

# creating dirs
mkdir kredyty
mkdir lokaty
mkdir zadania

# creating groups
sudo groupadd dbd
sudo groupadd dbb

# creating users - directors
sudo useradd dbd_director -p password
sudo useradd dbb_director -p password

# setting dirs acl
setfacl -d -m user::rw- kredyty
setfacl -d -m group::r-x kredyty
setfacl -m group:dbd:rwx kredyty
setfacl -m group:dbb:rwx kredyty
setfacl -m other::--- kredyty

setfacl -d -m user::rw- lokaty
setfacl -d -m group::r-x lokaty
setfacl -m group:dbd:rwx lokaty
setfacl -m group:dbb:rwx lokaty
setfacl -m other::--- lokaty

# setting directors acl
setfacl -d -m user:dbd_director:r kredyty
setfacl -d -m user:dbd_director:r lokaty
setfacl -d -m user:dbd_director:r zadania

setfacl -d -m user:dbb_director:r kredyty
setfacl -d -m user:dbb_director:r lokaty
setfacl -d -m user:dbb_director:r zadania

# creating users - employees
for ((i=0; i<number_of_employees; i++)); do
  if [ "${employees[$i, position]}" == "obsługa" ]; then
    employee_id="${employees[$i, id]}"
    employee_user_name="${employees[$i, name]}${employees[$i, surname]}${employee_id}"
    director_user_name="${employees[$i, departament]}_director"
    user_dir="zadania/${employee_id}"

    # creating a new user
    useradd -g "${employees[$i, departament]}" "${employee_user_name}" -p password

    # creating dir and changing ownership
    mkdir "${user_dir}"
    sudo chown "${employee_user_name}" "${user_dir}"

    # setting new dir acl for the director
    setfacl -m user:"${director_user_name}":rwx "${user_dir}"
    setfacl -d -m user:"${director_user_name}":rwx "${user_dir}"

    # setting new dir acl for the employee
    setfacl -d -m user:"${employee_user_name}":r-- "${user_dir}"

    # setting new dir acl for groups
    setfacl -d -m group:dbd:--- "${user_dir}"
    setfacl -d -m group:dbb:--- "${user_dir}"
  fi
done


echo -e "Testing...\n"

echo -e "Test a):"
for ((i=0; i<number_of_employees; i++)); do
  if [ "${employees[$i, position]}" == "obsługa" ]; then
    employee_id="${employees[$i, id]}"
    user_dir="zadania/${employee_id}"

    if [ "${employees[$i, departament]}" == "dbd" ]; then
      sudo su dbd_director -c "touch ${user_dir}/zadanie1"
      sudo su dbd_director -c "echo kredyt > ${user_dir}/zadanie1"

      sudo su dbd_director -c "touch ${user_dir}/zadanie2"
      sudo su dbd_director -c "echo lokata > ${user_dir}/zadanie2"
    fi
  fi
done
echo -e "done\n"

echo -e "Test b):"
for ((i=0; i<number_of_employees; i++)); do
  if [ "${employees[$i, position]}" == "obsługa" ]; then
    employee_id="${employees[$i, id]}"
    user_dir="zadania/${employee_id}"

    if [ "${employees[$i, departament]}" == "dbb" ]; then
      sudo su dbb_director -c "touch ${user_dir}/zadanie1"
      sudo su dbb_director -c "echo kredyt > ${user_dir}/zadanie1"

      sudo su dbb_director -c "touch ${user_dir}/zadanie2"
      sudo su dbb_director -c "echo kredyt > ${user_dir}/zadanie2"

      sudo su dbb_director -c "touch ${user_dir}/zadanie3"
      sudo su dbb_director -c "echo lokata > ${user_dir}/zadanie3"
    fi
  fi
done
echo -e "done\n"

echo -e "Test c):"
for ((i=0; i<number_of_employees; i++)); do
  if [ "${employees[$i, position]}" == "obsługa" ]; then
    employee_id="${employees[$i, id]}"
    user_dir="zadania/${employee_id}"

    if [ "${employees[$i, departament]}" == "dbb" ]; then
      echo "[test] permission should be denied:"
      sudo su dbd_director "cat ${user_dir}/zadanie1"
    fi
  fi
done
echo -e "done\n"

echo -e "Test d):"
for ((i=0; i<number_of_employees; i++)); do
  if [ "${employees[$i, position]}" == "obsługa" ]; then
    employee_id="${employees[$i, id]}"
    user_dir="zadania/${employee_id}"

    if [ "${employees[$i, departament]}" == "dbd" ]; then
      echo "[test] permission should be denied:"
      sudo su dbb_director -c "cat ${user_dir}/zadanie1"
    fi
  fi
done
echo -e "done\n"

echo -e "Test e):"
for ((i=0; i<number_of_employees; i++)); do
  if [ "${employees[$i, position]}" == "obsługa" ]; then
    employee_id="${employees[$i, id]}"
    employee_user_name="${employees[$i, name]}${employees[$i, surname]}${employee_id}"
    user_dir="zadania/${employee_id}"

    sudo su "${employee_user_name}" -c "cat ${user_dir}/zadanie1" > /dev/null
    sudo su "${employee_user_name}" -c "cat ${user_dir}/zadanie2" > /dev/null

    if [ "${employees[$i, departament]}" == "dbb" ]; then
      sudo su "${employee_user_name}" -c "cat ${user_dir}/zadanie3" > /dev/null
    fi

    echo "[test] permission should be denied:"
    sudo su "${employee_user_name}" -c "echo zrobione >> ${user_dir}/zadanie1"
    echo "[test] permission should be denied:"
    sudo su "${employee_user_name}" -c "echo zrobione >> ${user_dir}/zadanie2"

    if [ "${employees[$i, departament]}" == "dbb" ]; then
      echo "[test] permission should be denied:"
      sudo su "${employee_user_name}" -c "echo zrobione >> ${user_dir}/zadanie3"
    fi
  fi
done
echo -e "done\n"

echo -e "Test f):"
for ((i=0; i<number_of_employees; i++)); do
  if [ "${employees[$i, position]}" == "obsługa" ]; then
    employee_id="${employees[$i, id]}"
    employee_user_name="${employees[$i, name]}${employees[$i, surname]}${employee_id}"
    user_dir="zadania/${employee_id}"

    if [ "${employees[$i, departament]}" == "dbd" ]; then
      # "kredyt" in file "zadanie1.txt"
      sudo su "${employee_user_name}" -c "touch kredyty/zadanie${employee_id}_1"
      sudo su "${employee_user_name}" -c "echo zrobione > kredyty/zadanie${employee_id}_1"

      # "lokata" in file "zadanie2.txt"
      sudo su "${employee_user_name}" -c "touch lokaty/zadanie${employee_id}_2"
      sudo su "${employee_user_name}" -c "echo zrobione > lokaty/zadanie${employee_id}_2"
    else
      # "kredyt" in file "zadanie1.txt"
      sudo su "${employee_user_name}" -c "touch kredyty/zadanie${employee_id}_1"
      sudo su "${employee_user_name}" -c "echo zrobione > kredyty/zadanie${employee_id}_1"

      # "kredyt" in file "zadanie2.txt"
      sudo su "${employee_user_name}" -c "touch kredyty/zadanie${employee_id}_2"
      sudo su "${employee_user_name}" -c "echo zrobione > kredyty/zadanie${employee_id}_2"

      # "lokata" in file "zadanie3.txt"
      sudo su "${employee_user_name}" -c "touch lokaty/zadanie${employee_id}_3"
      sudo su "${employee_user_name}" -c "echo zrobione > lokaty/zadanie${employee_id}_3"
    fi
  fi
done
echo -e "done\n"

echo -e "Test g):"
for ((i=0; i<number_of_employees; i++)); do
  if [ "${employees[$i, position]}" == "obsługa" ]; then
    employee_id="${employees[$i, id]}"
    employee_user_name="${employees[$i, name]}${employees[$i, surname]}${employee_id}"

    echo "[test] employee id: ${employee_id}, permission should be denied if user id is different"

    for file in kredyty/*; do
      sudo su "${employee_user_name}" -c "echo zrobione >> ${file}"
    done

    for file in zadania/**/*; do
      sudo su "${employee_user_name}" -c "echo zrobione >> ${file}"
    done
  fi
done
echo -e "done\n"
