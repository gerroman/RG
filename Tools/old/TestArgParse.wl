#!/bin/env wolfram_script.sh
Needs["RG`Tools`"];

note[argparse[]]
forceFlag = argparse["force", False];
order = argparse["order", 1];
x = argparse["x", 0.1];
