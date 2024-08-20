BeginPackage["RG`IOUtils`", {"RG`Tools`"}];


load::usage = "load[file, symbol, definitions] load from file (if it exists) or execute symbol's definitions and save it to file"
loadFigure::usage = "loadFigure[fname, expr] load figure from file if it exists or execute expr and save figure to the file"


Begin["`Private`"]


path`run = makeDirectory[path`run];

Options[load] = {
  "verbose" -> True,
  "force" -> False,
  "path" -> path`run
};

load::get = "loading '`1`' ...";
load::save = "saving '`1`' ...";
load::failed = "can not find '`1`'";
load::evaluate = "evaluating `1` ... ";
load::hash = "hash differs '`1`' ...";

SetAttributes[load, HoldRest];
load[fname_] := With[{
		path = FileNameJoin[{OptionValue[load, "path"], ToString@fname}],
    verbose = OptionValue[load, "verbose"]
	},
	If[Not@FileExistsQ[path],
    error[StringForm[load::failed, path]];
    Return[$Failed];
  ];
	If[verbose, log[StringForm[load::get, path], "prefix"->"[load]: "]];
	Get[path]
];
load[fname_, expr_, OptionsPattern[]] := Module[{
		path = FileNameJoin[{OptionValue[load, "path"], ToString@fname}],
    hashpath = FileNameJoin[{OptionValue[load, "path"], ToString@fname <> ".hash"}],
    verbose = OptionValue[load, "verbose"],
    force = OptionValue[load, "force"],
    result = Unevaluated[expr],
    hash = Hash[Unevaluated[expr]]
	},
  If[force || Not@FileExistsQ[path] || Not@FileExistsQ[hashpath],
		If[verbose, log[StringForm[load::evaluate, HoldForm@@result], "prefix"->"[load]: "]];
		result = expr;
		If[verbose, log[StringForm[load::save, path], "prefix"->"[save]: "]];
		Put[result, path];
		If[verbose, log[StringForm[load::save, hashpath], "prefix"->"[save]: "]];
    Put[hash, hashpath];
  ];
  If[hash =!= Get[hashpath], 
    error[StringForm[load::hash, hashpath]];
    Return[$Failed];
  ];
	load[fname]
];

path`figs = makeDirectory[path`figs];
Options[loadFigure] = {
  "verbose" -> True,
  "force" -> False,
  "path" -> path`figs
};
SetAttributes[loadFigure, HoldRest];
loadFigure[fname_] := With[{
		path = FileNameJoin[{OptionValue[loadFigure, "path"], ToString@fname}],
    verbose = OptionValue[loadFigure, "verbose"]
	},
	If[Not@FileExistsQ[path],
    error[StringForm[load::failed, path]];
    Return[$Failed];
  ];
	If[verbose, log[StringForm[load::get, path], "prefix"->"[load]: "]];
	If[$Notebooks, Import[path], path]
];

loadFigure[fname_, expr_, OptionsPattern[]] := Module[{
		path = FileNameJoin[{OptionValue[loadFigure, "path"], ToString@fname}],
    hashpath = FileNameJoin[{OptionValue[loadFigure, "path"], ToString@fname <> ".hash"}],
    verbose = OptionValue[loadFigure, "verbose"],
    force = OptionValue[loadFigure, "force"],
    result = Unevaluated[expr],
    hash = Hash[Unevaluated[expr]]
	},
  If[force || Not@FileExistsQ[path] || Not@FileExistsQ[hashpath],
		If[verbose, log[StringForm[load::evaluate, HoldForm@@result], "prefix"->"[load]: "]];
		result = expr;
		If[verbose, log[StringForm[load::save, path], "prefix"->"[save]: "]];
    installFrontEnd[];
		Export[path, result];
		If[verbose, log[StringForm[load::save, hashpath], "prefix"->"[save]: "]];
    Put[hash, hashpath];
  ];
  If[hash =!= Get[hashpath], 
    error[StringForm[load::hash, hashpath]];
    Return[$Failed];
  ];
	loadFigure[fname]
];


End[]


note[path`run];
note[path`figs];


EndPackage[]

(* Local Variables: *)
(* mode: wl *)
(* compile-command: "math -script RG/SyntaxChecker/check.wl *.wl" *)
(* End: *)
