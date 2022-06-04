#!/usr/bin/env bash

echo "BSK 4 1." >> /home/manager1/file1
echo "BSK 4 2." >> /home/manager2/file2

groupadd fuse
chmod g+rw /dev/fuse
chgrp fuse /dev/fuse

gpasswd -a manager1 fuse
gpasswd -a manager2 fuse
