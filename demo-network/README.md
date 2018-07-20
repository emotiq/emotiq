# Emotiq demo blockchain network on localhost

Using scripts located here, we can run 3 Emotiq nodes bound to 127.0.0.1.
To avoid conflicts, each node uses its own set of TCP ports:

1. Node 1 ports:
  * 3140 REST API
  * 3145 WebSockets API
  * 65000 Gossip communications
1. Node 2 ports:
  * 3141 REST API
  * 3146 WebSockets API
  * 65001 Gossip communications
1. Node 3 ports:
  * 3142 REST API
  * 3147 WebSockets API
  * 65002 Gossip communications

Configuration files for this nodes are located in `var/etc` folder from the root of the repository.

Each node runs in its own `tmux` session. Sessions are named `node1`, `node2`, `node3` respectively.

## Requirements

To run the network, following tools have to be installed:
* tmux
* rlwrap
* netcat

They can be installed using following shell commands:

* **macOS**
  ```bash
  brew install tmux rlwrap netcat
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
