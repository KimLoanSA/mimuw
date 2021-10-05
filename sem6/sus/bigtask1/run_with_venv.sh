#!/usr/bin/env bash

venv_name="sus"

if [[ $# -eq 1 ]]; then
  echo -e "Creating virtual environment..."
  python3 -m venv $venv_name
  echo -e "Virtual environment created!"

  echo -e "Activating virtual environment..."
  source $venv_name/bin/activate
  echo -e "Virtual environment activated!"

  echo -e "Installing required dependencies..."
  pip3 install -r requirements.txt
  echo -e "Installing required dependencies..."

  echo -e "Running..."
  python3 characters_clustering.py "$1"

else
  echo -e "Usage: $0 path_to_the_input_file"
  exit 2
fi