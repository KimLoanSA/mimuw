FROM debian

RUN apt update
RUN apt install vim -y
RUN apt install swi-prolog -y


WORKDIR jpp/

COPY ma406058.pl ma406058.pl
COPY programs/ programs/
COPY runTests.sh runTests.sh

ENTRYPOINT ["./runTests.sh"]
