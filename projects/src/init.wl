#!/usr/bin/env wolfram_script.sh

BeginPackage["src`", {"RG`Tools`"}];

Get[FileNameJoin@{"src", "init-definitions.wl"}];
note[x];

Get[FileNameJoin@{"src", "init-paths.wl"}];
note[path`run, ExpandFileName];

EndPackage[];
