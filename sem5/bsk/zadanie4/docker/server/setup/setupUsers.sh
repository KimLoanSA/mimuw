#!/usr/bin/env bash

sudo useradd -m worker1
echo "worker1:worker1" | chpasswd

sudo useradd -m worker2
echo "worker2:worker2" | chpasswd

sudo useradd -m worker3
echo "worker3:worker3" | chpasswd

sudo useradd -m manager1
echo "manager1:manager1" | chpasswd

sudo useradd -m manager2
echo "manager2:manager2" | chpasswd
