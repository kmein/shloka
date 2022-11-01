#!/bin/sh
HERE=$(dirname "$0")
for book in $(seq 1 18); do
  wget "$(printf "https://bombay.indology.info/mahabharata/text/ASCII/MBh%02d.txt" "$book")" -P "$HERE"/../text
done
