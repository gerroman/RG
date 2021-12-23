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


hold::usage = "
  hold[pattern] apply HoldForm to matches of pattern
  hold[{x1,...}] apply HoldForm to specific xs
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


SetAttributes[hold, HoldAll];
hold[xs_List] := With[{
    rules = Join[
      Thread[-xs -> -Thread[HoldForm[xs]]],
      Thread[xs -> Thread[HoldForm[xs]]]
    ]
  },
  ReplaceAll[rules]
];
hold[pattern_] := Function[expr,
  With[{xs = Union@Cases[{expr}, pattern, Infinity]},
    hold[xs][expr]
  ]
];
hold[xs__] := hold[{xs}];


End[];

Echo[$Context];

EndPackage[];
