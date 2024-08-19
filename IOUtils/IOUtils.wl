BeginPackage["RG`IOUtils`", {"RG`Tools`"}];


load::usage = "load[file, symbol, definitions] load from file (if it exists) or execute symbol's definitions and save it to file"
loadFigure::usage = "loadFigure[fname, expr] load figure from file if it exists or execute expr and save figure to the file"


Begin["`Private`"]


SetAttributes[load, HoldRest];


Options[load] = {"update" -> False, "verbose" -> False, "force" -> False};


load::get = "[info]: loading `1` ...";
load::save = "[info]: saving `1` ...";
load::failed = "[error]: failed to load '`1`'";

load[fname_, symbol_Symbol, expr_] := With[{
		path = FileNameJoin[{Global`temporarydirectory, ToString@fname}]
	},
	If[FileExistsQ[path] && Not[OptionValue["update"]], (
			If[OptionValue["verbose"],
				Print[ToString[StringForm[load::get, path]]];
			];
			Get[path]
		), (
			expr;
			If[OptionValue["verbose"],
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
			If[OptionValue["verbose"],
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
Options[loadFigure] = {"update" -> False, "verbose" -> True, "force"->False};
loadFigure[fname_String, expr_, OptionsPattern[]] := With[{
		path = FileNameJoin[{Global`figuredirectory, fname}],
		hash = Hash[Hold[expr]],
		hashpath = FileNameJoin[{Global`figuredirectory, fname}] <> ".hash",
		force = OptionValue["force"]
	},
	If[(Not@FileExistsQ[path]
			|| Not@FileExistsQ[hashpath]
			|| (hash =!= Get[hashpath] && OptionValue["update"])), (
			If[OptionValue["verbose"],
				print[ToString[StringForm[load::save, path]]];
				print[ToString[StringForm[load::save, hashpath]]];
			];
			Export[path, expr];
			Put[hash, hashpath];
	)];
	If[hash =!= Get[hashpath],
		print["[warning]: expression hashs differ, consider force update ..."];
		If[force,
			print["[warning]: forcing update ... "];
			If[OptionValue["verbose"],
				print[ToString[StringForm[load::save, path]]];
				print[ToString[StringForm[load::save, hashpath]]];
			];
			Export[path, expr];
			Put[hash, hashpath];
		]
	];
	Import[path];
	path
];
loadFigure[fname_String] := With[{
		path = FileNameJoin[{Global`figuredirectory, fname}]
	},
	If[FileExistsQ[path], Import[path],
		print[StringForm["[error]: '``' not found", path]];
		path
	]
];


End[]


EndPackage[]
