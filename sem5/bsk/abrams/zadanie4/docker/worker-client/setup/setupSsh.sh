#!/usr/bin/env bash

chown -R worker1:worker1 /home/worker1/.ssh
chmod 600 /home/worker1/.ssh/id_rsa
chmod 600 /home/worker1/.ssh/id_rsa.pub
chmod 700 /home/worker1/.ssh

chown -R worker2:worker2 /home/worker2/.ssh
chmod 600 /home/worker2/.ssh/id_rsa
chmod 600 /home/worker2/.ssh/id_rsa.pub
chmod 700 /home/worker2/.ssh

chown -R worker3:worker3 /home/worker3/.ssh
chmod 600 /home/worker3/.ssh/id_rsa
chmod 600 /home/worker3/.ssh/id_rsa.pub
chmod 700 /home/worker3/.ssh
