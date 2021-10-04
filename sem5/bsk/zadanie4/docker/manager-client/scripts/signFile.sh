#!/usr/bin/env bash

gpg --sign --pinentry-mode=loopback "$1"
