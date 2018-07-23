#!/usr/bin/env bash

node_id=${1:-1}
tmux_logging_plugin_path=~/.tmux/plugins/tmux-logging

tmux run-shell -t node${node_id} "${tmux_logging_plugin_path}/scripts/save_complete_history.sh"
