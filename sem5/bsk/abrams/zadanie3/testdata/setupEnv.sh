#!/usr/bin/env bash

sudo addgroup managers
sudo addgroup workers

sudo useradd manager1
echo "manager1:manager1" | chpasswd
sudo usermod -a -G managers manager1
sudo usermod -a -G workers manager1

sudo useradd worker1
echo "worker1:worker1" | chpasswd
sudo usermod -a -G workers worker1

sudo useradd worker2
echo "worker2:worker2" | chpasswd
sudo usermod -a -G workers worker2

sudo useradd worker3
echo "worker3:worker3" | chpasswd

chmod a+rwx -R klasyfikacja/

echo -e "worker1 ALL=(ALL) NOPASSWD: $(pwd)/zadanie_3" | sudo EDITOR='tee -a' visudo