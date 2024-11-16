#!/usr/bin/env wolfram_script.sh

Needs["RG`Tools`"];
Needs["src`"];

results = {fname`result};

If[checkExists[results], exit[0]];

result = x + 3;

llog["saving results", 
  Put[result, fname`result];
];

exit[checkExists[results]];
