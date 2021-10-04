FROM debian-bsk-4

COPY ssh/sshd_config /etc/ssh/sshd_config

COPY setup/ setup/

RUN ./setup/setupUsers.sh

COPY ssh/worker1/authorized_keys /home/worker1/.ssh/authorized_keys
COPY ssh/worker2/authorized_keys /home/worker2/.ssh/authorized_keys
COPY ssh/worker3/authorized_keys /home/worker3/.ssh/authorized_keys
COPY ssh/manager1/authorized_keys /home/manager1/.ssh/authorized_keys
COPY ssh/manager2/authorized_keys /home/manager2/.ssh/authorized_keys

ENTRYPOINT ["bash"]
