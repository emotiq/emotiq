#!/usr/bin/env bash

github-release upload \
  --tag ${CI_COMMIT_TAG} \
  --name $(basename $1) \
  --file $1
