#!/usr/bin/env bash

apt update -y
apt install iptables -y

iptables-legacy -A INPUT -m iprange --src-range 172.16.12.1-172.16.13.253 -j REJECT
