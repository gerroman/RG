(* ::Package:: *)

BeginPackage["RG`BaseUtils`"];


(* ::Text:: *)
(*Common options: update, verbose.*)


update::usage = "
  update -> True option to force call definitions and save to file in load[]
";
verbose::usage = "
  verbose -> True make function more verbose
";


(* ::Text:: *)
(*Environment: temporary-, figure-, and working- directories*)


temporarydirectory::usage = "
  temporarydirectory return location to save auxiliary files
";

figuredirectory::usage = "
  figuredirectory return location to save figures
";

workingdirectory::usage = "
  workingdirectory \[LongDash] current working directory
";


(* ::Text:: *)
(*Load definitions and figures from files*)


load::usage = "
  load[file, symbol, definitions] load from file (if it exists) or
  execute symbol's definitions and save it to file
";

loadFigure::usage = "
  loadFigure[fname, expr] load figure from file if it exists or execute expr and save figure to the file
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
Echo["[Info]: set temporary directory to " <> temporarydirectory];

workingdirectory = Check[NotebookDirectory[], temporarydirectory];
Echo["[Info]: set working directory to " <> workingdirectory];
SetDirectory[workingdirectory];

figuredirectory = workingdirectory;
Echo["[Info]: set figure directory to " <> figuredirectory];


SetAttributes[load, HoldAll];
Options[load] = {update -> False, verbose -> False};
load::get = "[Info] load `1` ...";
load::save = "[Info] save `1` ...";
load::failed = "[Error] failed to load file `1`";
load[fname_String, symbol_Symbol, expr___, OptionsPattern[]] := With[{
    path = FileNameJoin[{temporarydirectory, fname}]
	},
  If[FileExistsQ[path] && Not[OptionValue[update]], (
      If[OptionValue[verbose],
			  Echo[ToString[StringForm[load::get, path]]]
			];
      Get[path]
    ), (
      expr;
			If[OptionValue[verbose],
        Echo[ToString[StringForm[load::save, path]]]
			];
      DumpSave[path, symbol];
    )
  ]
];

load[fname_String, OptionsPattern[]] := With[{
    path = FileNameJoin[{temporarydirectory, fname}]
	},
  If[FileExistsQ[path],
    (
		  If[OptionValue[verbose],
			  Echo[ToString[StringForm[load::get, path]]]
			];
			Get[path]
		),
    (
			Message[load::failed, path];
			Null
		)
  ]
]


SetAttributes[loadFigure, HoldAll];
Options[loadFigure] = {update -> False, verbose -> False};
loadFigure[fname_String, expr_, OptionsPattern[]] := With[{
    path = FileNameJoin[{figuredirectory, fname}]
	},
  If[Not@FileExistsQ[path] || OptionValue[update], (
      If[OptionValue[verbose],
			  Echo[ToString[StringForm[load::save, path]]]
			];
      Export[path, expr];
	)];
	If[OptionValue[verbose],
	  Echo[ToString[StringForm[load::get, path]]]
	];
	Import[path]
];


SetAttributes[off, HoldAll];
off[message__, expr_] := (
  Off[message];
	With[{result = expr},
	  On[message];
		result
	]
);


SetAttributes[hold, HoldAll];
hold[xs_List] := With[{
    rule = Join[
		  Thread[Thread[HoldForm[xs]] -> Thread[HoldForm[xs]]],
      Thread[-xs -> -Thread[HoldForm[xs]]],
      Thread[xs -> Thread[HoldForm[xs]]]
    ]
  },
  ReplaceRepeated[#, rule]&
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
