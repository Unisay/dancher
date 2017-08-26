#!/usr/bin/env bash

pulp -w browserify \
  --jobs 4 \
  --transform aliasify \
  --to static/bundle.js
