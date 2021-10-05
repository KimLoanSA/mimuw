# big task 1

Simple program that clusters characters using DBSCAN algorithm.

## How to Run

---
**Requirements:**
- python3
- pip3
- virtualenv

---
**Simple Way:**
```bash
./run_with_venv.sh <path_to_the_input_file>
```
!!! While running on docker you will need to install:
```bash
RUN apt-get update
RUN apt-get install ffmpeg libsm6 libxext6 -y
```

**By Hand:**
```bash
python3 -m venv <venv name>
source <venv name>/bin/activate

pip3 install -r requirements.txt
python3 characters_clustering.py <path_to_the_input_file>
```

---
**Program parameters**:

Program can be configured with startup arguments.

Positional arguments:
- `images_list_file_path` - path to file with listed character images  _(required)_

Optional arguments:
- `-h, --help` - show help message and exit
- `--output_file OUTPUT_FILE` - name of file with the final clustering _(default: 'result.txt')_
- `--output_html_file OUTPUT_HTML_FILE` - name of HTML file with the final clustering _(default: 'result.html')_
- `--metric METRIC` - DBSCAN metric value _(default: 'euclidean')_
- `--eps EPS` - DBSCAN eps value _(default: 720)_
- `--n_jobs N_JOBS` - DBSCAN n_jobs value _(default: -1)_
- `--image_additional_padding IMAGE_ADDITIONAL_PADDING` - Minimal padding added to images _(default: 60)_

**Default values can be changed in the `character_clustering/defaults.py` file.**


## Used libraries

- **numpy** - basic fast array operations
- **DBSCAN** (sklearn.cluster) - Clustering algorithm, fast and does *not* require number of clusters
- **pyhtml** - Easy HTML generation
- **OpenCV** - reading images
