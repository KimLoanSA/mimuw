#!/usr/bin/env bash

# script generates input file to the clustering program
# takes path to directory with input images and creates file with list of input images paths

if [[ $# -eq 2 ]]; then
  directory_path=$(realpath "$1")
  output_file=$2

  echo -e "generating the '${output_file}' file with images from the '${directory_path}' directory..."

  cat /dev/null > "${output_file}"

  for file in "${directory_path}"/*; do
    echo "${file}" >> "${output_file}"
  done

  echo "done!"
else
  echo -e "Usage: $0 path_to_directory output_file_name"
  exit 2
fi