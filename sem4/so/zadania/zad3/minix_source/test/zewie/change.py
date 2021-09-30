#!/usr/bin/env python3

import sys

lines = open(sys.argv[1], "r").readlines()

mapy = {}

for line in lines:
	if "MAPA" in line:
		a, b, c = line.strip().split(" ")
		mapy[b] = c
	if "FATAL" in line:
		sys.stderr.write("HEJ COŚ JEST ŹLE BO " + line)
		exit(1)

for line in sorted(lines):
	for key, val in mapy.items():
		line = line.replace(key, val)
	print(line, end="")


