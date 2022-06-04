FROM ubuntu:18.04

WORKDIR wbo/
USER root

RUN apt update -y
RUN apt install python3 -y
RUN apt install python3-pip -y
RUN apt install clustalw -y
RUN apt install muscle -y

RUN pip3 install biopython
RUN pip3 install requests
RUN pip3 install pylab-sdk

COPY src/ src/
COPY entrypoint.sh entrypoint.sh

ENTRYPOINT ["./entrypoint.sh"]
