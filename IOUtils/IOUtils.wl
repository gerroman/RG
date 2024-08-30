BeginPackage["RG`IOUtils`", {"RG`Tools`"}];


load::usage = "load[fname] \[LongDash] load expression from file 'fname'";
export::usage = "export[file, expr, opts] export from file (if it exists) or execute symbol's definitions and save it to file"

loadFigure::usage = "loadFigure[fname] \[LongDash] load figure from file 'fname'";
exportFigure::usage = "exportFigure[fname, expr] export figure from file if it exists or execute expr and save figure to the file"


Begin["`Private`"]


load::load = "loading '`1`' ...";
load::failed = "can not find '`1`'";

Options[load] = {
	"verbose" -> True
};

load[fname_] := With[{
		fnameFull = ToString@fname,
		verbose = OptionValue[export, "verbose"]
	},
	If[Not@FileExistsQ[fnameFull],
		error[StringForm[load::failed, fnameFull]];
		Return[$Failed];
	];
	If[verbose, log[StringForm[load::load, fnameFull], "prefix"->"[load]: "]];
	Get[fnameFull]
];

Options[loadFigure] = {
	"verbose" -> True
};
loadFigure[fname_] := With[{
		fnameFull = ToString@fname,
		verbose = OptionValue[loadFigure, "verbose"]
	},
	If[Not@FileExistsQ[fnameFull],
		error[StringForm[load::failed, fnameFull]];
		Return[$Failed];
	];
	If[verbose, log[StringForm[load::load, fnameFull], "prefix"->"[load]: "]];
	If[$Notebooks, Import[fnameFull], fnameFull]
];


export::load = "loading '`1`' ...";
export::save = "saving '`1`' ...";
export::failed = "can not find '`1`'";
export::evaluate = "evaluating `1` ... ";
export::hash = "hash differs '`1`' ...";

Options[export] = {
	"verbose" -> True,
	"force" -> False,
  "comments" :> DateString[]
};
SetAttributes[export, HoldRest];
export[fname_, expr_, OptionsPattern[]] := Module[{
		fnameFull = ToString@fname,
		fnameHash = ToString@fname <> ".hash",
		verbose = OptionValue[export, "verbose"],
		force = OptionValue[export, "force"],
		result = Unevaluated[expr],
		hash = Hash[Unevaluated[expr]],
    comments = OptionValue[export, "comments"]
	},
	If[force || Not@FileExistsQ[fnameFull] || Not@FileExistsQ[fnameHash],
		If[verbose, log[StringForm[export::evaluate,
      StringTrim[StringPadRight[ToString[InputForm@result], $MessageLength]]], "prefix"->"[export]: "
    ]];
		result = expr;
		If[verbose, log[StringForm[export::save, fnameFull], "prefix"->"[save]: "]];
		Export[fnameFull, result, "Comments" -> comments];
		If[verbose, log[StringForm[export::save, fnameHash], "prefix"->"[save]: "]];
		Put[hash, fnameHash];
	];
	If[hash =!= Get[fnameHash],
		error[StringForm[export::hash, fnameHash]];
		Return[$Failed];
	];
	load[fname]
];


Options[exportFigure] = {
	"verbose" -> True,
	"force" -> False
};
SetAttributes[exportFigure, HoldRest];
exportFigure[fname_, expr_, opts:OptionsPattern[{exportFigure, Export, Graphics, Plot, Rasterize}]] := Module[{
		fnameFull = ToString@fname,
		fnameHash =ToString@fname <> ".hash",
		verbose = OptionValue[exportFigure, "verbose"],
		force = OptionValue[exportFigure, "force"],
		result = Unevaluated[expr],
		hash = Hash[Unevaluated[expr]],
		exportOpts = FilterRules[{opts}, Options[exportFigure]],
		exportOpts = FilterRules[{opts}, Join[Options[Export], Options[Rasterize]]],
		graphicsOpts = FilterRules[{opts}, Join[Options[Graphics], Options[Plot]]]
	},
	If[force || Not@FileExistsQ[fnameFull] || Not@FileExistsQ[fnameHash],
		If[verbose, log[StringForm[export::evaluate, StringTrim[StringPadRight[ToString[InputForm@result], $MessageLength]]], "prefix"->"[export]: "]];
		result = expr;
		If[verbose, log[StringForm[export::save, fnameFull], "prefix"->"[save]: "]];
		installFrontEnd[];
		Export[fnameFull, If[Head[result] === Graphics, Show[result, Sequence@@graphicsOpts], result], Sequence@@exportOpts, ImageFormattingWidth->Infinity, Background->None];
		If[verbose, log[StringForm[export::save, fnameHash], "prefix"->"[save]: "]];
		Put[hash, fnameHash];
	];
	If[hash =!= Get[fnameHash],
    error[StringForm[export::hash, fnameHash]];
	  Return[$Failed];
  ];
	loadFigure[fname]
];


End[]


EndPackage[]
