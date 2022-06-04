#!/usr/bin/env bash

openvpn --proto tcp-client --dev tun --tls-client --ifconfig 10.32.0.7 10.32.0.6 \
--ca config/vpn/ca.crt \
--cert config/vpn/vpn-client.crt \
--key config/vpn/vpn-client.key \
--remote 10.32.0.69
