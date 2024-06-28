#!/bin/env wolfram_script.sh
Needs["RG`Tools`"];

pprint[argparse[]]

forceFlag = argparse["force", False];
pprint[forceFlag];

order = argparse["order", 1];
pprint[order];

x = argparse["x", 0.1];
pprint[x];
