FROM debian-bsk-4

COPY config/hosts /etc/hosts
COPY config/fuse.conf /etc/fuse.conf
COPY setup/ setup/

RUN ./setup/setupUsers.sh

COPY ssh/manager1/id_rsa /home/manager1/.ssh/id_rsa
COPY ssh/manager1/id_rsa.pub /home/manager1/.ssh/id_rsa.pub

COPY ssh/manager2/id_rsa /home/manager2/.ssh/id_rsa
COPY ssh/manager2/id_rsa.pub /home/manager2/.ssh/id_rsa.pub

COPY gpg/manager1/ /home/manager1/gpg/
COPY gpg/manager2/ /home/manager2/gpg/
COPY gpg/importKeys.sh /home/manager1/gpg/importKeys.sh
COPY gpg/importKeys.sh /home/manager2/gpg/importKeys.sh

COPY scripts/mountBankData.sh /home/manager1/mountBankData.sh
COPY scripts/signFile.sh /home/manager1/signFile.sh
COPY scripts/verifyFile.sh /home/manager1/verifyFile.sh

COPY scripts/mountBankData.sh /home/manager2/mountBankData.sh
COPY scripts/signFile.sh /home/manager2/signFile.sh
COPY scripts/verifyFile.sh /home/manager2/verifyFile.sh

RUN ./setup/setupSsh.sh

ENTRYPOINT ["bash"]
