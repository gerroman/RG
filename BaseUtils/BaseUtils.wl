(* ::Package:: *)

BeginPackage["RG`BaseUtils`"];


load::usage = "
  load[file, symbol, definitions] load from file (if it exists) or
  execute symbol's definitions and save it to file
";


temporary\[LetterSpace]directory::usage = "
  temporary\[LetterSpace]directory return location to save auxiliary files
";


verbose::usage = "
  verbose -> True option to make function more verbose
";


off::usage = "
  off[message, expr] evaluate expression with the message temporally off 
";


Begin["`Private`"];


Protect[verbose];


temporary\[LetterSpace]directory = FileNameJoin[{$TemporaryDirectory, "RG"}];
If[Not[FileExistsQ[temporary\[LetterSpace]directory]],
  CreateDirectory[temporary\[LetterSpace]directory]
];


SetAttributes[load, HoldAll];
load::get = "Getting `1` ...";
load::save = "Saving `1` ...";
load::failed = "Error: failed to load file `1`";
load[fname_, symbol_, definition_] := With[{path = FileNameJoin[{temporary\[LetterSpace]directory, fname}]},
  If[FileExistsQ[path], (
      PrintTemporary[ToString[StringForm[load::get, path]]];
      Get[path]
    ),(
      definition;
      PrintTemporary[ToString[StringForm[load::save, path]]];
      DumpSave[path, symbol];
    )
  ]
];

load[fname_] := With[
  {path = FileNameJoin[{temporary\[LetterSpace]directory, fname}]},
  If[FileExistsQ[path],
    (PrintTemporary@StringForm[load::get, path]; Get[path]),
    PrintTemporary@StringForm[load::failed, path]
  ]
]


SetAttributes[off, HoldAll];
off[message_, expr_] := Module[{result},
   Off[message];
   result = expr;
   On[message];
   Return[result];
];


End[];


EndPackage[];