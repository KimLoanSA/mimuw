FROM 32bit/debian

RUN apt update
RUN apt install gdb --force-yes -y
RUN apt install git --force-yes -y
RUN apt install binutils --force-yes -y

RUN git clone https://github.com/longld/peda.git ~/peda
RUN echo "source ~/peda/peda.py" >> ~/.gdbinit

RUN apt-get install cowsay --force-yes -y
RUN /usr/games/cowsay "Done! Moooooooo"
