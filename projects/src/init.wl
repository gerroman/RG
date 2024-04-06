#!/usr/bin/env wolfram_script.sh

Needs["RG`Tools`"];
Get[FileNameJoin@{"src", "init-definitions.wl"}];
Get[FileNameJoin@{"src", "init-paths.wl"}];

note[path`run, ExpandFileName];
