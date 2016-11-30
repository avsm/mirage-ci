#!/bin/sh

NAME=mirage-ci.logs
EXTRA="opam-repository mirage-dev"
cd data/datakit
sudo rm -rf data
mkdir data
cd data
git init
git checkout -b github-metadata
git remote add origin git@github.com:mirage/${NAME}
for i in ${EXTRA}; do
  mkdir -p mirage/$i
  touch mirage/$i/.monitor
  git add mirage/$i/.monitor
  git commit -m "Monitor mirage/$i"
done
