#!/usr/bin/env bash

gpg --import pub.gpg
gpg --pinentry-mode loopback --import private.gpg
