#!/usr/bin/env wolfram_script.sh

Needs["RG`Tools`"];
Get["src`"];

paths = Sort[ToExpression/@Names["path`*"]];

llog["making directories",
  Scan[
    If[Not@FileExistsQ[#], 
      CreateDirectory[#], 
      log[StringForm["`` already exist", #], prefix->"\n[....]: ", endl->" ... "]
    ]&, 
    paths
  ];
];
Write["stdout", paths // StringReplace[{path`run~~x__ :> "."~~x}]];

exit[0];
