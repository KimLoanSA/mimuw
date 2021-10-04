#!/usr/bin/env bash

groupadd fuse
chmod g+rw /dev/fuse
chgrp fuse /dev/fuse

gpasswd -a worker1 fuse
gpasswd -a worker2 fuse
gpasswd -a worker3 fuse
