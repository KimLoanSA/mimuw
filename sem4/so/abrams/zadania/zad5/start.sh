#!/bin/sh

qemu-system-x86_64 -curses \
    -drive file=minix-machine.img \
    -rtc base=localtime \
    -net user,hostfwd=tcp::15881-:22 \
    -net nic,model=virtio \
    -m 1024M \
    -drive file=extra.img,format=raw,index=1,media=disk
