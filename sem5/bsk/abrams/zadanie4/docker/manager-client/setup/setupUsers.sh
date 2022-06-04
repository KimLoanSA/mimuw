#!/usr/bin/env bash

sudo useradd -m manager1
echo "manager1:manager1" | chpasswd

sudo useradd -m manager2
echo "manager2:manager2" | chpasswd
