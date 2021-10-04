#!/usr/bin/env bash

# firewall rules

iptables-legacy -t nat -A PREROUTING -p tcp --dport 80 -j DNAT --to-destination 172.16.12.69:8000
iptables-legacy -t nat -A POSTROUTING -j MASQUERADE

iptables-legacy -A INPUT -p tcp --dport 1194 -j ACCEPT
iptables-legacy -A INPUT -j DROP


openvpn --proto tcp-server --dev tun --tls-server --ifconfig 10.32.0.6 10.32.0.7 \
--ca config/vpn/ca.crt \
--cert config/vpn/vpn-server.crt \
--key config/vpn/vpn-server.key \
--dh config/vpn/dh.pem \
--log-append /var/log/openvpn.log
