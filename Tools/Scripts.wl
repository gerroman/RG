Off[General::shdw];


BeginPackage["RG`Scripts`"];


log::usage = "log[expr] \[LongDash] converts 'expr' to string and call Write[] or Print[] depending on batch or notebooks working mode";
error::usage = "error[expr] \[LongDash] log  'expr' with '[ERROR]' prefix";
warning::usage = "warning[expr] \[LongDash] log  'expr' with '[warning]' prefix";
echo::usage = "echo[expr] \[LongDash] prints and return expr";

timeStamp::usage = "timeStamp[] \[LongDash] print timeString[]";
systemStamp::usage = "systemStamp[] \[LongDash] print systemString[]";

Export::usage=System`Export::usage;
Timing::usage=System`Timing::usage;
Print::usage=System`Print::usage;

argparse::usage = "argparse[] \[LongDash] returns {argc, argv}";

info::usage = "info[func] \[LongDash] get information about func: context, usage, attributes, options
info[func, All] \[LongDash] get full information about func including up/down values";


head::usage = "head[fname] \[LongDash] return first line of the text file";
sizeOf::usage = "sizeOf[expr] \[LongDash] evaluates number of leafs and size in bytes of 'expr'";


Begin["`Private`"];


(* ::Section:: *)
(* Logging *)


Options[RG`Scripts`Print] = {
  "stream" -> "stderr",
  "colorize" -> {
  	"[info]" -> "\033[1;35m[info]\033[0m",
  	"[directory]" -> "\033[1;35m[directory]\033[0m",
  	"[....]" -> "\033[1;35m[....]\033[0m",
  	"[usage]" -> "\033[1;37m[usage]\033[0m",
  	"[load]" -> "\033[1;37m[load]\033[0m",
  	"[file]" -> "\033[1;35m[file]\033[0m",
  	"[hash]" -> "\033[1;35m[hash]\033[0m",
  	"[export]" -> "\033[1;37m[export]\033[0m",
  	"[ERROR]" -> "\033[1;31m[ERROR]\033[0m",
  	"[time]" -> "\033[1;35m[time]\033[0m",
  	"[date]" -> "\033[1;35m[date]\033[0m",
  	"[seconds]" -> "\033[1;35m[seconds]\033[0m",
  	"[test]" -> "\033[1;34m[test]\033[0m",
  	"[OK]" -> "\033[1;32m[OK]\033[0m",
  	"[note]" -> "\033[1;33m[note]\033[0m",
  	"[echo]" -> "\033[1;33m[echo]\033[0m",
  	"[running]" -> "\033[1;36m[running]\033[0m",
  	"[exit]" -> "\033[1;36m[exit]\033[0m",
  	"[RESULT]" -> "\033[1;31m[RESULT]\033[0m",
  	"[init]" -> "\033[1;36m[init]\033[0m",
  	"[warning]" -> "\033[0;33m[warning]\033[0m",
  	"[args]" -> "\033[1;34m[args]\033[0m"
  },
  "width" :> 1000
};
RG`Scripts`Print[message_String, opts:OptionsPattern[]] := If[$Notebooks,
	System`Print[message],
	WriteString[
    OptionValue["stream"],
    StringJoin[
      "\r",
      StringReplace[message, OptionValue["colorize"]],
      "\n"
    ]
  ]
];
RG`Scripts`Print[expr_, opts:OptionsPattern[]] := If[$Notebooks,
	System`Print[expr],
  WriteString[
    OptionValue["stream"],
    StringJoin[
      "\r",
      ToString[expr, FormatType->InputForm, TotalWidth->OptionValue["width"]],
      "\n"
    ]
  ]
];
RG`Scripts`Print[expr__, opts:OptionsPattern[]] := Scan[RG`Scripts`Print[#, opts]&, {expr}];


Options[log] = {
	"prefix" -> "[info]: ",
	"endl" -> "",
  "width" -> 70,
  "column" -> False
};
log[expr_String, opts:OptionsPattern[]] := With[{
    message = StringJoin[OptionValue["prefix"], expr, OptionValue["endl"]]
	},
	RG`Scripts`Print[message];
];
log[expr_StringForm, opts:OptionsPattern[]] := With[{
    message = StringJoin[OptionValue["prefix"], ToString[expr], OptionValue["endl"]]
	},
	RG`Scripts`Print[message];
];
log[expr_List, opts:OptionsPattern[]] := If[OptionValue["column"],
Scan[log[#, opts]&, expr],
log[
  ToString[expr, FormatType->InputForm, TotalWidth->OptionValue["width"]],
  opts
]
];

log[expr_, opts:OptionsPattern[]] := log[
  ToString[expr, FormatType->InputForm, TotalWidth->OptionValue["width"]],
  opts
];
log[expr__, opts:OptionsPattern[]] := Scan[log[#, opts]&, {expr}];


error[expr_, opts:OptionsPattern[]] := log[expr, "prefix"->"[ERROR]: ", opts];
warning[expr_, opts:OptionsPattern[]] := log[expr, "prefix"->"[warning]: ", opts];
echo[expr_] := (logwrite[expr, "width"->Infinity]; expr);


(* ::Section:: *)
(* Time and System *)


timeString := DateString[{"<", "Year", "-", "Month", "-", "Day", " ", "Hour",":", "Minute", ":", "Second", ">"}];
timeStamp[] := With[{stamp = timeString},
	log[stamp, "prefix" -> "[date]: "];
];

systemString := ToString@StringForm["``@`` : Wolfram Mathematica ``", $UserName, $MachineName, $Version];
systemStamp[] := With[{stamp = systemString},
	log[stamp];
];


(* ::Section:: *)
(* Parsing command line arguments *)

argparse[] := Which[
	$ScriptCommandLine =!= {}, {Length[#], #}&[$ScriptCommandLine],
	Length[$CommandLine] >= 2 && $CommandLine[[2]] === "-script", {Length[#], #}&[$CommandLine[[3;;]]],
	True, {0, {}}
];


argparse[name_String, False] := Module[{argc, argv, value},
	{argc, argv} = argparse[];
	value = MemberQ[argv, "-" <> name];
	If[value,
		log[StringForm["`` = `` (command line flag)", name, value], "prefix"->"[args]: "],
		log[StringForm["`` = `` (absent command line flag)", name, value], "prefix"->"[args]: "]
	];
	Return[value]
];

argparse[name_String, default:(_Integer|_Real|_String)] := Module[
	{argc, argv, pos, value},
	{argc, argv} = argparse[];
	pos = Position[argv, "-" <> name];
	If[Length[pos] != 1 || pos[[1, 1]] + 1 > argc,
		log[StringForm["`` = `` (default value)", name, default], "prefix"->"[args]: "];
		Return[default];
	];
	value = ToExpression[argv[[pos[[1, 1]] + 1]]];
	If[Head[value] =!= Head[default],
		log[StringForm["`` = `` (using default value)", name, default], "prefix"->"[args]: "];
		log[StringForm["'``' is not a '``'", argv[[pos[[1, 1]] + 1]], Head[default]], "prefix"->"[warning]: "];
    Return[default];
  ];
	log[StringForm["`` = `` (command line argument, default = ``)", name, value, default], "prefix"->"[args]: "];
	Return[value];
];


(* ::Section:: *)
(* Exporting results *)

RG`Scripts`Export::export = "saving '`1`' to the file '`2`' ... ";
RG`Scripts`Export::hashError = "hash differs ... ";
RG`Scripts`Export::hashCorrect = "hash is the same";

Options[RG`Scripts`Export] = Join[Options[System`Export], {
	"force" -> False,
	"Comments" -> ""
}];
RG`Scripts`Export[fname_, expr_, opts:OptionsPattern[]] := Module[{
		fnameFull = ToString[fname],
		fnameHash = ToString[fname] <> ".hash",
		force = OptionValue["force"],
		hash = Hash[expr],
		comments = StringRiffle[{
				"[comments]: " <> ToString@OptionValue["Comments"],
				"[author]: " <> systemString,
				"[date]: " <> timeString
			}, {"", " *)\n(* ", ""}
		],
    exportOpts = FilterRules[{opts}, Options[System`Export]]
	},
	log[fnameFull];
	If[force || Not@FileExistsQ[fnameFull] || Not@FileExistsQ[fnameHash],
		log[StringForm[RG`Scripts`Export::export, ToString[expr, TotalWidth->300],  fnameFull], "prefix"->"[export]: "];
	 	System`Export[fnameFull, expr, "Comments"->comments, Sequence@@exportOpts];
    log["complete", "prefix"->"[export]: "];
		log[StringForm[RG`Scripts`Export::export, hash, fnameHash], "prefix"->"[export]: "];
		Put[hash, fnameHash];
    log["complete", "prefix"->"[export]: "];
	];
	If[hash === Get[fnameHash],
		log[RG`Scripts`Export::hashCorrect, "prefix"->"[hash]: "],
	 	(
			error[RG`Scripts`Export::hashError];
	 		Return[$Failed];
		)
	];
  Return[fnameFull];
] /; StringMatchQ[FileExtension[ToString[fname]], {"m", "wl"}];


(* ::Section:: *)
(* Timing *)

Options[RG`Scripts`Timing] = {"verbose"->False};
SetAttributes[RG`Scripts`Timing, HoldFirst];
RG`Scripts`Timing[expr_] := Module[{time, result},
  log[ToString[Unevaluated[expr]], "prefix"->"[time]: ", "endl"->" ... "];
  {time, result} = If[OptionValue[RG`Scripts`Timing, "verbose"], AbsoluteTiming[expr], Block[{Print}, AbsoluteTiming[expr]]];
  log[ToString@NumberForm[time, {6, 2}] <> " [seconds]", "prefix"->"[time]: "];
  Return[{time, result}];
];



(* ::Section:: *)
(* Information *)

emptyQ[expr_] := Length[expr] == 0;

SetAttributes[info, {HoldAll, Listable}];
info[expr_Symbol, None] := (
	log[If[ValueQ[expr::usage],
			expr::usage,
			ToString@StringForm["[warning]: `` is undefined", expr::usage]
		]
	];
);
info[expr_Symbol] := (
	log[Context[expr]];
  If[ValueQ[expr::usage], log[expr::usage, "prefix"->"[usage]: ", "width"->Infinity]];
	If[Not[emptyQ[Attributes[expr]]], log[Attributes[expr], "width"->Infinity]];
	If[Not[emptyQ[Options[expr]]], log[Options[expr], "column"->True, "prefix"->"[option]: "]];
);
info[expr_Symbol, All] := (
	info[expr];
	If[Not[emptyQ[UpValues[expr]]],log[UpValues[expr]]];
	If[Not[emptyQ[OwnValues[expr]]], log[OwnValues[expr]]];
	If[Not[emptyQ[DownValues[expr]]], log[DownValues[expr]]];
);
info[expr_String] := log[Names[expr]];



head[fname_String] := Module[{stream, result = $Failed},
	If[FileExistsQ[fname],
		(
      log[StringForm["\"``\" (`` bytes)", fname, FileByteCount[fname]], "prefix"->"[file]: "];
			stream = OpenRead[fname];
			result = ReadLine[stream];
			Close[stream];
		),
		log[StringForm["can not find '``'", fname], "prefix"->"[error]: "];
	];
	Return[result];
];
head[fname_String, n_Integer] := Module[{stream, result = $Failed},
	If[FileExistsQ[fname],
		(
      log[StringForm["\"``\" (`` bytes)", fname, FileByteCount[fname]], "prefix"->"[file]: "];
			stream = OpenRead[fname];
			result = StringRiffle[
				Table[ReadLine[stream], n] // DeleteCases[EndOfFile],
				{"", "\n", ""}
			];
			Close[stream];
		),
		log[StringForm["can not find '``'", fname], "prefix" -> "[error]: "];
	];
	Return[result];
];


sizeOf[expr_] := Module[
	{leafs = LeafCount[expr], bytes = Quantity[ByteCount[expr], "Bytes"]},
	bytes = 1`3 * Which[
		QuantityMagnitude[bytes] > 10^9, UnitConvert[bytes, "Gigabytes"],
		QuantityMagnitude[bytes] > 10^6, UnitConvert[bytes, "Megabytes"],
		QuantityMagnitude[bytes] > 10^3, UnitConvert[bytes, "Kilobytes"],
		True, bytes
	];
	{leafs, bytes}
];




End[];


EndPackage[];


On[General::shdw];


