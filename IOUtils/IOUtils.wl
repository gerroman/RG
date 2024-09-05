BeginPackage["RG`IOUtils`", {"RG`Tools`"}];


load::usage = "load[fname] \[LongDash] load expression from file 'fname'";
export::usage = "export[file, expr, opts] export from file (if it exists) or execute symbol's definitions and save it to file"

loadFigure::usage = "loadFigure[fname] \[LongDash] load figure from file 'fname'";
exportFigure::usage = "exportFigure[fname, expr] export figure from file if it exists or execute expr and save figure to the file"


Begin["`Private`"]


load::failed = "can not find '`1`'";
Options[load] = {"verbose" -> False}; 
load[fname_, OptionsPattern[]] := With[{
		fnameFull = ToString@fname
	},
	If[Not@FileExistsQ[fnameFull],
		error[StringForm[load::failed, fnameFull]];
		Return[$Failed];
	];
	If[OptionValue["verbose"], head[fnameFull, 3] // print, head[fnameFull, 0]];
	Get[fnameFull] // llog
];

Options[loadFigure] = {"verbose" -> False}; 
loadFigure[fname_, OptionsPattern[]] := With[{
		fnameFull = ToString@fname
	},
	If[Not@FileExistsQ[fnameFull],
		error[StringForm[load::failed, fnameFull]];
		Return[$Failed];
	];
	If[OptionValue["verbose"], head[fnameFull, 3] // print, head[fnameFull, 0]];
	If[$Notebooks, Import[fnameFull] // llog, fnameFull]
];

export::save = "saving '`1`' ...";
export::failed = "can not find '`1`'";
export::export = "exporting `1` ... ";
export::hash = "hash differs '`1`' ...";
export::hashCorrect = "hash is the same as in '`1`' ...";

Options[export] = {
	"verbose" -> True,
	"force" -> False,
	"Comments" :> ""
};
export[fname_, expr_, OptionsPattern[]] := Module[{
		fnameFull = ToString@fname,
		fnameHash = ToString@fname <> ".hash",
		verbose = OptionValue[export, "verbose"],
		force = OptionValue[export, "force"],
		hash = Hash[expr],
		comments = StringRiffle[{
				"[comments]: " <> ToString@OptionValue[export, "Comments"],
				"[author]: " <> systemString[],
				"[date]: " <> timeString[]
			}, {"", " *)\n(* ", ""}
		]
	},
	If[force || Not@FileExistsQ[fnameFull] || Not@FileExistsQ[fnameHash],
		If[verbose, log[StringForm[export::export,
			StringTrim[StringPadRight[ToString[InputForm@expr], $MessageLength]]], "prefix"->"[export]: "
		]];
		If[verbose, log[StringForm[export::save, fnameFull], "prefix"->"[export]: "]];
		Export[fnameFull, expr, "Comments" -> comments];
		If[verbose, log[StringForm[export::save, fnameHash], "prefix"->"[export]: "]];
		Put[hash, fnameHash];
	];
	If[hash === Get[fnameHash],
		If[verbose, log[StringForm[export::hashCorrect, fnameHash], "prefix"->"[export]: "]],
		(
			error[StringForm[export::hash, fnameHash]];
			Return[$Failed];
		)
	];
	fname
];


Options[exportFigure] = {
	"verbose" -> True,
	"force" -> False
};
exportFigure[fname_, expr_, opts:OptionsPattern[{exportFigure, Export, Graphics, Plot, Rasterize}]] := Module[{
		fnameFull = ToString@fname,
		fnameHash =ToString@fname <> ".hash",
		verbose = OptionValue[exportFigure, "verbose"],
		force = OptionValue[exportFigure, "force"],
		result,	hash,
		exportFigureOpts = FilterRules[{opts}, Options[exportFigure]],
		exportOpts = FilterRules[{opts}, Join[Options[Export], Options[Rasterize]]],
		graphicsOpts = FilterRules[{opts}, Join[Options[Graphics], Options[Plot]]]
	},
	result = If[Head[expr] === Graphics, Show[expr, Sequence@@graphicsOpts], expr];
	hash = Hash[result];
	If[force || Not@FileExistsQ[fnameFull] || Not@FileExistsQ[fnameHash],
		If[verbose, log[StringForm[export::export, StringTrim[StringPadRight[ToString[InputForm@result], $MessageLength]]], "prefix"->"[export]: "]];
		If[verbose, log[StringForm[export::save, fnameFull], "prefix"->"[export]: "]];
		installFrontEnd[];
		Export[fnameFull, result, Sequence@@exportOpts, ImageFormattingWidth->Infinity];
		If[verbose, log[StringForm[export::save, fnameHash], "prefix"->"[export]: "]];
		Put[hash, fnameHash];
	];
	If[hash === Get[fnameHash],
		If[verbose, log[StringForm[export::hashCorrect, fnameHash], "prefix"->"[export]: "]],
		(
			error[StringForm[export::hash, fnameHash]];
			Return[$Failed];
		)
	];
	fname
];


End[]


EndPackage[]
