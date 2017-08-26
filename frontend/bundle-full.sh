#!/usr/bin/env bash

pulp browserify \
  --jobs 4 \
  --force \
  --optimise \
  --transform aliasify \
  | uglifyjs \
  --compress \
  --mangle \
  --verbose \
  --timings \
  --warn \
  -o ./static/bundle.js



