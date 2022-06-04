FROM python:3.8

WORKDIR sus/

COPY characters_clustering/ character_clustering/
COPY input/ input/
COPY characters_clustering.py characters_clustering.py
COPY requirements.txt requirements.txt
COPY run_with_venv.sh run_with_venv.sh

RUN apt-get update
RUN apt-get install ffmpeg libsm6 libxext6 -y

ENTRYPOINT ["bash"]