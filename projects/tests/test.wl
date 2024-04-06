#!/usr/bin/env wolfram_script.sh

Get[FileNameJoin[{"src", "init.wl"}]];
On[Assert];
result = Get[fname`result];
check[result == 4];
check[x == 2];
exit[0];
