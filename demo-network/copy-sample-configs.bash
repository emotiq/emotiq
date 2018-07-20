#!/usr/bin/env bash

BASE="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEVNET_CONFIGS_PATH=$BASE/../node-configs
EMOTIQ_ETC_ROOT=$BASE/../../var/etc

# Copy testing config files
mkdir -p $EMOTIQ_ETC_ROOT
cp -r $DEVNET_CONFIGS_PATH/* $EMOTIQ_ETC_ROOT
