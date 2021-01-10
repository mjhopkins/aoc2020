#!/bin/bash

cd "$(dirname "$0")"

if [ -r .cookie ]; then
  . .cookie
fi

export TZ=EST
thisyear="$(date +%Y)"
thismonth="$(date +%m)"
thisday="$(date +%d)"

year=2020
for day in {1..25}; do
  # if [ "$thisyear" -ne "$year" -o "$thismonth" -ne 12 -o "$day" -gt "$thisday" ]; then
  #   exit 0
  # fi
  filename=data/"$day".input
  if [ -r "$filename" ]; then
    echo "skipping $filename (exists)"
    continue  # make sure we don't fetch the same file twice!
  fi
  echo "Getting $day"
  curl -sS -o "$filename" https://adventofcode.com/"$year"/day/"$day"/input
  # curl -sS -o "$filename" -b "$AOC_COOKIE" https://adventofcode.com/"$year"/day/"$day"/input
done