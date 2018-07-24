# Emotiq demo blockchain network on localhost

Using scripts located here, we can run 3 Emotiq nodes bound to 127.0.0.1.
To avoid conflicts, each node uses its own set of TCP ports:

1. Node 1 ports:
  * 3140 - REST API
  * 3145 - WebSockets API
  * 65000 - Gossip communications
1. Node 2 ports:
  * 3141 - REST API
  * 3146 - WebSockets API
  * 65001 - Gossip communications
1. Node 3 ports:
  * 3142 - REST API
  * 3147 - WebSockets API
  * 65002 - Gossip communications

Configuration files for this nodes are located in `var/etc` folder from the root of the repository.

Each node runs in its own `tmux` session. Sessions are named `node1`, `node2`, `node3` respectively.

## Requirements

To run the network, following tools have to be installed:
* tmux
* rlwrap
* netcat

`macOS` also needs `coreutils` to be installed

They can be installed using following shell commands:

* **macOS**
  ```bash
  brew install tmux rlwrap netcat coreutils
  ```
* **Linux**
  ```bash
  sudo apt-get update && sudo apt-get install -y tmux rlwrap netcat
  ```

## (Optional) WebSockets API use

To use supplied Python scripts to communcate with node using WebSockets API, also Python3 interpreter and JSON RPC WebSockets modules are requiered. They can be installed by following shell commands:
* **macOS**
  ```bash
  brew install python3
  pip3 install -U -r requirements.txt
  ```
* **Linux**
  ```bash
  sudo apt-get update && sudo apt-get python3 python3-pip
  sudo pip3 install -U -r requirements.txt
  ```

## (Optional) Install `tmux-logging` tmux plugin to save tmux session logs to files

Use following `bash` commands to install `tmux-logging` plugin:
```bash
mkdir -p ~/.tmux/plugins
git clone https://github.com/tmux-plugins/tmux-logging ~/.tmux/plugins/tmux-logging
```
Add lines:
```
set -g history-limit 50000
run-shell ~/.tmux/plugins/tmux-logging/logging.tmux
```
On `macOS` its also recommended to install `ansifilter` utility:
```bash
brew install ansifilter
```

`tmux-logging` plugin places logs in $HOME directory.

## Running blockchain

### Setup configuration files for the nodes
:bangbang: **ATTENTION!!! Overwrites `var/etc/{node1,node2,node3}` directories** :bangbang:
```bash
./copy-sample-configs.bash
```
:bangbang: **ATTENTION!!! Overwrites `var/etc/{node1,node2,node3}` directories** :bangbang:

This script copies configurations from `node-configs` directory to `var/etc`.

### Start 3-node blockchain
```bash
./start-blockchain.bash
```

This script starts 3 nodes in the separate `tmux` sessions `node1`, `node2` and `node3`
One can attach to the node Lisp REPL using following command:
```bash
tmux a -t node1   # for Node 1
```

### (Experimental) Start blockchain with CCL IDE running 1st node
```bash
./start-blockchain-with-ide.bash
```

**ATTENTION** When buffer pops up, press `Command-Shift-E` to start node.
CCL Cocoa IDE assumed to be named `Clozure CL64.app` (change )



### Run single WebSockets ping to each node
```bash
./ping-nodes.bash
```

### Stop all nodes
```bash
./stop-blockchain.bash
```

### Toggle `tmux` session logging for a node
```bash
./toggle_logging.bash <node_id>
```
Example for `node1`:
```bash
./toggle_logging.bash 1
```
Run the same command to stop logging.

### Save session history for a node:
```bash
./save-history.bash <node_id>
```
Example for `node1`:
```bash
./save-history.bash 1
```
