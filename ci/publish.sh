#!/usr/bin/env bash

cd var/local/production
version=$(cat version.txt)
tarball=emotiq-${version}.tar.bz2
tar cvfj ${tarball} emotiq-${version}

github-release upload \
  --tag ${CI_COMMIT_TAG} \
  --name ${tarball} \
  --file ${tarball}
