# Dockerfile do budowania obrazu z testami, base obrazem jest obraz z pld-linux z zainstalowanymi bibliotekami
FROM mimuw.sik.abrams/pld-linux-libs:1_0

COPY solve testhttp/solve/
COPY docker-test/ testhttp/docker-test/
COPY dockerTestsEntrypoint.sh testhttp/dockerTestsEntrypoint.sh

ENTRYPOINT ["testhttp/dockerTestsEntrypoint.sh"]