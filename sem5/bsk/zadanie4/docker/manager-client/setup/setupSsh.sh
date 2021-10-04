#!/usr/bin/env bash

chown -R manager1:manager1 /home/manager1/.ssh
chmod 600 /home/manager1/.ssh/id_rsa
chmod 600 /home/manager1/.ssh/id_rsa.pub
chmod 700 /home/manager1/.ssh

chown -R manager2:manager2 /home/manager2/.ssh
chmod 600 /home/manager2/.ssh/id_rsa
chmod 600 /home/manager2/.ssh/id_rsa.pub
chmod 700 /home/manager2/.ssh
