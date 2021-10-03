# Dockerfile do budowania obrazu z pld-linux z zainstalowanymi bibliotekami
FROM registry.gitlab.com/pld-linux/pld

RUN poldek --shcmd="install gcc"
RUN poldek --shcmd="install make"
RUN poldek --shcmd="install glibc-devel"
RUN poldek --shcmd="install diffutils"

ENTRYPOINT bin/bash