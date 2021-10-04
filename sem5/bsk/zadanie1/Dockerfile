FROM debian

RUN apt update
RUN apt install sudo -y
RUN apt install acl
RUN apt install rsyslog -y
RUN apt install systemd -y

WORKDIR /bsk

COPY src/script.sh script.sh
COPY test/ test/
COPY entrypoint.sh entrypoint.sh

ENTRYPOINT ["./entrypoint.sh"]

