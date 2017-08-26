#!/usr/bin/env bash

echo "Dancher: building a fully optimized bundle"

pulp \
  --monochrome \
  browserify \
  --jobs 4 \
  --force \
  --optimise \
  --transform aliasify \
  | uglifyjs \
  --compress \
  --mangle \
  --timings \
  -o ./static/bundle.js

echo "Bundle is ready: "
ls -lah ./static/bundle.js
