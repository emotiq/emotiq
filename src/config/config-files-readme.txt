Config files:

keypairs.conf
Public/private keypair(s) on this machine.
There could be more than 1 pair in this file for purposes of simulating
nodes on a single machine. This also serves as the lookup database that
allows a node to lookup its private key given its public key. No secrecy
of private keys is assumed for now, because we're just simulating.
Every machine gets the same file.
In a real (non-simulated) network, there would only be one keypair in this file
and every machine gets a different file.

hosts.conf
Known domain names and ports (seeds) of machines that participate.
Every machine gets the same file. (Although this is not strictly required.
It's possible some machines know about hosts that other machines don't know about.
Gossip can spread this information at startup.)

pubkeys.conf
Known public keys of witnesses. Every machine gets the same file.

gossip.conf
Gossip network configuration on local machine. Contains pubkeys
that will be simulated on THIS machine only. Each pubkey here MUST
also have a matching entry in keypairs.conf.
Every machine gets a different file.

Startup process:
Local machine reads gossip.conf then sends a :list-keys message to the gossip port
of every machine (except itself) in hosts.conf. Every node on every machine responds
with its public key. This is how every machine deduces the mapping between the public
keys in pubkeys.conf and hosts.conf.

At this point, every machine knows both the local public keys and domain name
of every other machine in the network.