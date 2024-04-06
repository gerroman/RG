#!/usr/bin/env wolfram_script.sh

Get["src`"];

result = x + 3;
llog["saving results", Put[result, fname`result]];

exit[0];
