To test the Cosi network:

1. In file cosi-construction.lisp there is a defvar *LOCAL-NODES* that
describes the available IP Nodes in your local system. These nodes can
become hosts of this Cosi code, communicating with the others, and run
a number of simulated nodes to effect a Cosi network subtree. Edit
*LOCAL-NODES* as needed.

Initially, these true nodes will each become Cosi subtree leaders, and
one of them, desgnated by *LEADER-NODE* should be designated overall
leader for the Cosi network.  

2. The first time you run the Cosi simulator, you should decide on the
size of the Cosi network and generate a configuration file that
describes all the simulation nodes and partitions them among the real
nodes from (1).

You do that by running GENERATE-TREE, which defaults to 1000
simulation nodes, plus the real nodes you described in (1). This
generates two output files into othe ./config directory. One contains
the network partitioning that was randomly generated, and the other
describes the associations of public / private crypto keys used by the
simulation nodes for generating signatures.

3. Thereafter, you can just load up the existing network configuration
files using RECONSTRUCT-TREE.

4. To run the Cosi network, in file cosi-handlers.lisp, first
initialize the simulator with INIT-SIM, then run TST. If every real
node participates then the resulting multi-signature should have a
census equal to the whole size of the simulator. But if only some
nodes participate, a multi-signature should still be generated with a
smaller census.

DM/Emotiq 03/18
