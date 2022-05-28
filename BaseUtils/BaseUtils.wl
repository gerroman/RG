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
on::usage = "
  on[message, expr] evaluate expression with the message temporally on
";


hold::usage = "
  hold[pattern] apply HoldForm to matches of pattern
  hold[{x1,...}] apply HoldForm to specific xs
";


Begin["`Private`"];


Protect[verbose];
Protect[update];

SetAttributes[load, HoldAll];
Options[load] = {update -> False, verbose -> False};
load::get = "[Info]: load `1` ...";
load::save = "[Info]: save `1` ...";
load::failed = "[Error]: failed to load file `1`";
load[fname_String, symbol_Symbol, expr___, OptionsPattern[]] := With[{
    path = FileNameJoin[{Global`temporarydirectory, fname}]
	},
  If[FileExistsQ[path] && Not[OptionValue[update]], (
      If[OptionValue[verbose],
			  Print[ToString[StringForm[load::get, path]]];
			];
      Get[path]
    ), (
      expr;
			If[OptionValue[verbose],
        Print[ToString[StringForm[load::save, path]]];
			];
      DumpSave[path, symbol];
    )
  ]
];


load[fname_String, OptionsPattern[]] := With[{
    path = FileNameJoin[{Global`temporarydirectory, fname}]
	},
  If[FileExistsQ[path],
    (
		  If[OptionValue[verbose],
			  Print[ToString[StringForm[load::get, path]]];
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
    path = FileNameJoin[{Global`figuredirectory, fname}]
	},
  If[Not@FileExistsQ[path] || OptionValue[update], (
      If[OptionValue[verbose],
			  Print[ToString[StringForm[load::save, path]]];
			];
      Export[path, expr];
	)];
	If[OptionValue[verbose],
	  Print[ToString[StringForm[load::get, path]]];
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

SetAttributes[on, HoldAll];
on[message__, expr_] := (
  On[message];
	With[{result = expr},
	  Off[message];
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


Print[$Context];


EndPackage[];
