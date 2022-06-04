#!/usr/bin/env bash

./setupEnv.sh

cmake .
make

echo -e "\n\n\n==================\n"

echo -e "\n\n-- logged as worker1 --"
su worker1 -c "./zadanie_3"

echo -e "\n\n-- logged as worker1 --"
su worker1 -c "./zadanie_3"

echo -e "\n\n-- logged as worker1 --"
su worker1 -c "./zadanie_3"

echo -e "\n\n-- logged as worker2 --"
su worker2 -c "./zadanie_3"

echo -e "\n\n-- logged as worker3 --"
su worker3 -c "./zadanie_3"

echo -e "\n\n-- logged as manager1 --"
su manager1 -c "./zadanie_3"

echo -e "\n\n-- logged as manager1 --"
su manager1 -c "./zadanie_3"

echo -e "\n\n-- logged as worker1 with sudo --"
su worker1 -c "sudo ./zadanie_3"

echo -e "\n\n-- logged as worker2 with sudo --"
su worker2 -c "sudo ./zadanie_3"