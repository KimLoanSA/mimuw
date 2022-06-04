FROM debian-bsk-4

COPY config/hosts /etc/hosts
COPY setup/ setup/

RUN ./setup/setupUsers.sh

COPY ssh/worker1/id_rsa /home/worker1/.ssh/id_rsa
COPY ssh/worker1/id_rsa.pub /home/worker1/.ssh/id_rsa.pub

COPY ssh/worker2/id_rsa /home/worker2/.ssh/id_rsa
COPY ssh/worker2/id_rsa.pub /home/worker2/.ssh/id_rsa.pub

COPY ssh/worker3/id_rsa /home/worker3/.ssh/id_rsa
COPY ssh/worker3/id_rsa.pub /home/worker3/.ssh/id_rsa.pub

COPY scripts/mountBankData.sh /home/worker1/mountBankData.sh
COPY scripts/mountBankData.sh /home/worker2/mountBankData.sh
COPY scripts/mountBankData.sh /home/worker3/mountBankData.sh

RUN ./setup/setupSsh.sh

ENTRYPOINT ["bash"]
