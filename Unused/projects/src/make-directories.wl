#!/usr/bin/env wolfram_script.sh

Needs["RG`Tools`"];
Needs["src`"];

paths = Sort[ToExpression/@Names["path`*"]];

If[checkExists[paths], exit[0]];

llog["making directories",
  Scan[
    If[Not@FileExistsQ[#],
      CreateDirectory[#],
      log[StringForm["'``' already exist", #], prefix->"\n[....]: ", endl->""]
    ]&,
    paths
  ]
];

exit[checkExists[paths]];
