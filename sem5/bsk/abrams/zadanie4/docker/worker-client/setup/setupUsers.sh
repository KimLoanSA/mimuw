#!/usr/bin/env bash

sudo useradd -m worker1
echo "worker1:worker1" | chpasswd

sudo useradd -m worker2
echo "worker2:worker2" | chpasswd

sudo useradd -m worker3
echo "worker3:worker3" | chpasswd
