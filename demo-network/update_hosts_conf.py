#!/usr/bin/env python

import os
import sys


def get_base_dir():
    os.chdir(os.path.dirname(os.path.realpath(__file__))+"/..")
    return os.getcwd()


def write_hosts_conf(path, count):
    with open(path + "/hosts.conf", "w") as f:
        for i in range(1, count+1):
            f.write("(\"127.0.0.1\" %d)\n" % (64999+i))


base_dir = get_base_dir()

if len(sys.argv) > 1:
    nodes_count = int(sys.argv[1])
else:
    nodes_count = 1

for i in range(1, nodes_count+1):
    write_hosts_conf(base_dir+"/var/etc/node%d" % (i), nodes_count)
