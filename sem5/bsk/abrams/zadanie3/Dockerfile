FROM debian-bsk-3

WORKDIR /bsk-3

COPY src/ src/
COPY CMakeLists.txt CMakeLists.txt

COPY testdata/setupEnv.sh setupEnv.sh
COPY testdata/kredyty kredyty
COPY testdata/klasyfikacja klasyfikacja

COPY entrypoint.sh entrypoint.sh

COPY config/bsk_3_manager /etc/pam.d/bsk_3_manager
COPY config/bsk_3_worker /etc/pam.d/bsk_3_worker

ENTRYPOINT ["./entrypoint.sh"]