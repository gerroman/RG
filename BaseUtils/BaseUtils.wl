(* ::Package:: *)


BeginPackage["RG`BaseUtils`"];


load::usage = "
  load[file, symbol, definitions] load from file (if it exists) or
  execute symbol's definitions and save it to file
";
update::usage = "
  update -> True option to force call definitions and save to file in load[]
";

temporarydirectory::usage = "
  temporarydirectory return location to save auxiliary files
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
Protect[update];


temporarydirectory = FileNameJoin[{$TemporaryDirectory, "RG"}];
If[Not[FileExistsQ[temporarydirectory]],
  CreateDirectory[temporarydirectory]
];


SetAttributes[load, HoldAll];
Options[load] = {update -> False};
load::get = "[Info] load `1` ...";
load::save = "[Info] save `1` ...";
load::failed = "[Error] failed to load file `1`";
load[fname_String, symbol_Symbol, expr_, OptionsPattern[]] := With[{
    path = FileNameJoin[{temporarydirectory, fname}]
	},
  If[FileExistsQ[path] && Not[OptionValue[update]], (
      Echo[ToString[StringForm[load::get, path]]];
      Get[path]
    ), (
      expr;
      Echo[ToString[StringForm[load::save, path]]];
      DumpSave[path, symbol];
    )
  ]
];

load[fname_String] := With[{
    path = FileNameJoin[{temporarydirectory, fname}]
	},
  If[FileExistsQ[path],
    (
		  Echo[ToString[StringForm[load::get, path]]];
			Get[path]
		),
    (
		  Echo[ToString[StringForm[load::failed, path]]];
			Null
		)
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
    rule = Join[
      Thread[-xs -> -Thread[HoldForm[xs]]],
      Thread[xs -> Thread[HoldForm[xs]]]
    ]
  },
  ReplaceAll[rule]
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
