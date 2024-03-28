(* ::Package:: *)

(* ::Section:: *)
(*Tools*)


BeginPackage["RG`Tools`"];


verbose::usage = "verbose -> True \[LongDash] option to make function to be more verbose."


reset::usage = "reset[] \[LongDash] terminate session"


print::usage = "print[expr] evaluate first expr, print result"


pprint::usage = "pprint[expr] - call print[HoldForm[expr] == expr]
pprint[expr, func] - call print[HoldForm[expr] == func[expr]]
pprint[expr, func, opts] - call print[HoldForm[expr] == func[expr], opts]"


info::usage = "info[func] \[LongDash] get information about func: context, usage, attributes, options
info[func, All] \[LongDash] get full information about func including up/down values"


installFrontEnd::usage = "installFrontEnd[] \[LongDash] install auxiliary FrontEnd for wolfram scripts"


uninstallFrontEnd::usage = "uninstallFrontEnd[] \[LongDash] uninstall auxiliary FrontEnd wolfram scripts"


show::usage = "show[expr] \[LongDash] use auxiliary FrontEnd to present result of expr"


present::usage = "present[expr] \[LongDash] forms an equation: HoldForm[expr] == expr"


rewriteIt::usage = "rewriteIt[func][expr] \[LongDash] rewrite expr (equation, rule) using func for the right hand side
rewriteIt[funcL, funcR][expr] \[LongDash] rewrite expr (equation, rule) using funcL for the left hand sideand funcR for the right hand side"


modify::usage = "modify[pattern, func] create function to replace all matches of the pattern to results of application of the function func to these matches
modify[{x1, ...}, func] create function for specific x1, ..."


processList::usage = "processList[fs][expr] apply list of functions to expression, returns results of all steps"


process::usage = "process[fs][expr] apply list of functions to expression, returns {expr, final result}"


partition::usage = "partition[list, n] \[LongDash] partition list to n onoverlapping sublists of length n, and (if necessary) appends the rest elements"


ffirst::usage = "ffirst[list] return first element of flattened list
ffirst[list, verbose -> False] suppress warnings"


flast::usage = "flast[list] return last element of flattened list
flast[list, verbose -> False] suppress warnings"


emptyQ::usage = "emptyQ[list] returns True for empty list {}"

cases::usage = "cases[pattern] \[LongDash] function to get union expressions matching 'pattern'
cases[expr_, patter_] call cases[pattern][expr]
"

makeDirectory::usage = "makeDirectory[path] \[LongDash] creates 'path' if it does note exists"

silent::usage = "silent[expr] \[LongDash] evaluates 'expr' with turned off Print"

log::usage = "log[expr] \[LongDash] converts 'expr' to string and write an info message to 'stderr'"

timing::usage = "timing[expr] \[LongDash] timing 'expr' and write an info message to 'stderr'"

check::usage = "check[expr] \[LongDash] check 'expr' to be True, write an info message to 'stderr'"

exit::usage = "exit[code] \[LongDash] exit with code from script"

note::usage = "note[expr] \[LongDash] converts 'expr' (HoldForm input and InputForm output) to strings and write an info message to 'stderr'"


sizeOf::usage = "sizeOf[expr] \[LongDash] evaluates number of leafs and size in bytes of 'expr'"

$Colorize::usage = "$Colorize = True to colorize output (default: 'False' for Windows, 'True' for Linux)"


install::usage = "install[fname] \[LongDash] install 'fname.ext' adding correct extension from 'bin' directory
install[fname, path] \[LongDash] install 'fname.ext' adding correct extension using 'path' as directory
"

error::usage = "error[expr] \[LongDash] log  'expr' with '[ERROR]' prefix"

warning::usage = "warning[expr] \[LongDash] log  'expr' with '[warning]' prefix"

llog::usage = "llog[message_, expr_] \[LongDash] prints 'message', call 'expr', print and '[OK]' at the end"


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Kernel working and logging*)


$Colorize = ($OperatingSystem === "Unix" && Not[$Notebooks]);


reset[exitcode_:0] := (
	log["closing wolfram session ..."];
	exit[exitcode];
);


SetAttributes[print, {HoldFirst}];
Options[print] = {
	"verbose"->False,
	"stream"->"stderr",
	"header" :> DateString[{"(* ", "Year", "/", "Month", "/", "Day", " : ", "Hour",":", "Minute", ":", "Second", " *)\n"}],
	"sep"->"\n"
};
print[expr_, opts:OptionsPattern[]] := print[expr, Identity, opts];
print[expr_, func_, opts:OptionsPattern[]] := With[{
		stream=OptionValue["stream"],
		sep=OptionValue["sep"],
		verbose=OptionValue["verbose"]
	},
	If[verbose, WriteString[stream, ToString@OptionValue["header"]]];
	WriteString[stream, (expr // func // ToString)];
	WriteString[stream, sep];
];


SetAttributes[pprint, {HoldFirst, Listable}];
pprint[expr_, opts:OptionsPattern[]] := print[
	HoldForm[expr] // rewriteIt[ReleaseHold]
	, opts
];
pprint[expr_, func_, opts:OptionsPattern[]] := print[
	HoldForm[expr] // rewriteIt[ReleaseHold, func]
	, opts
];


SetAttributes[info, {HoldAll, Listable}];
info[expr_Symbol, None] := (
	print[If[ValueQ[expr::usage],
			expr::usage,
			ToString@StringForm["[warning]: `` is undefined", expr::usage]
		]
	];
);
info[expr_Symbol] := (
	pprint[Context[expr]];
	print[expr::usage, verbose->False];
	If[Not[emptyQ[Attributes[expr]]], pprint[Attributes[expr], Column, verbose->False]];
	If[Not[emptyQ[Options[expr]]], pprint[Options[expr], Column, verbose->False]];
);
info[expr_Symbol, All] := (
	info[expr];
	If[Not[emptyQ[UpValues[expr]]], pprint[UpValues[expr], Column, verbose->False]];
	If[Not[emptyQ[OwnValues[expr]]], pprint[OwnValues[expr], Column, verbose->False]];
	If[Not[emptyQ[DownValues[expr]]], pprint[DownValues[expr], Column, verbose->False]];
);
info[expr_String] := (
	print[Names[expr] // partition[#, 4]&, Column];
);


cases[patt_] := Function[expr, Union@Cases[expr, patt, Infinity]];
cases[expr_, patt_] := Union@Cases[expr, patt, Infinity];



(* ::Subsection:: *)
(*FrontEnd operations from scripts*)


installFrontEnd[serverFlag_:False] := With[{link = System`UseFrontEndDump`$felink},
	If[Not@MemberQ[Links[], link], Developer`InstallFrontEnd["Server"->serverFlag]];
	Links[]
];


uninstallFrontEnd[] := With[{link = System`UseFrontEndDump`$felink},
	If[MemberQ[Links[], link], LinkClose[link]];
	Links[]
];


SetAttributes[show, {HoldFirst, Listable}];
Options[show] = {WindowTitle:>ToString[StringForm["Out[``]", $Line]]};
show[expr_, func_:Identity, opts:OptionsPattern[]] := With[
	{wrapper = If[$Notebooks, Identity, UsingFrontEnd]},
	wrapper[
		CreateDialog[
			Column[{
				Labeled[Framed[func[expr], FrameMargins->10, ImageMargins->10, RoundingRadius->5], HoldForm[expr], Top],
				DefaultButton[]
			}, Alignment->Center],
			WindowTitle->OptionValue[WindowTitle]
		]
	]
];


(* ::Subsection:: *)
(*Equations*)


SetAttributes[present, HoldFirst];
present[expr_] := HoldForm[expr] == expr;


processList::unused = "function '``' has no effect in processList";
processList[fs_List][expr_] := With[{result = FoldList[#2[#1]&, expr, fs]},
	If[Not @ DuplicateFreeQ[result],
		Module[{prev, pos},
			pos = LengthWhile[result, With[{test = (# =!= prev)}, (prev=#;test)]&];
			log[ToString@StringForm[processList::unused, InputForm[fs[[pos]]]], prefix->"[ERROR]: "];
		];
	];
	result
];
processList[fs__][expr_] := processList[{fs}][expr];

Options[process] = {verbose -> False};
process[fs_List, opts:OptionsPattern[]][expr_] := If[OptionValue[verbose],
	processList[fs][expr][[{1, -1}]],
	{expr, (RightComposition@@fs)[expr]}
];
process[fs__][expr_] := process[{fs}][expr];


rewriteIt[lfunc_List, rfunc_List][l_List] := rewriteIt[lfunc, rfunc] /@ l;
rewriteIt[lfunc_List, rfunc_List][(h:(Equal|Rule))[lhs_, rhs_]] := With[{
		x = (RightComposition @@ lfunc)[lhs],
		y = (RightComposition @@ rfunc)[rhs]
	},
	h[x, y]
];
rewriteIt[lfunc_List, rfunc_List][expr_] := With[{
		x = (RightComposition @@ lfunc)[expr],
		y = (RightComposition @@ rfunc)[expr]
	},
	x == y
];
rewriteIt[fs__][expr_] := rewriteIt[{Identity}, {fs}][expr];


modify[xs_List, func_] := With[{
		rules = Thread[Rule[xs, Map[func, xs]]]
	},
	ReplaceAll[rules]
];
modify[pattern_, func_] := Function[
	expr,
	With[{xs = Union@Cases[{expr}, pattern, Infinity]},
		modify[xs, func][expr]
	]
];


(* ::Subsection:: *)
(*Lists*)


ffirst::argx = "List `1` contains contains more than one element"


Options[ffirst] = {"verbose" -> True};
ffirst[expr_List, OptionsPattern[]] := Block[{flat = Flatten[expr]},
	If[OptionValue[verbose] && Length[flat] > 1, Message[ffirst::argx, flat]];
	First[flat]
];


flast::argx = "List `1` contains contains more than one element"


Options[flast] = {"verbose" -> True};
flast[expr_List, OptionsPattern[]] := Block[{flat = Flatten[expr]},
	If[OptionValue[verbose] && Length[flat] > 1, Message[flast::argx, flat]];
	Last[flat]
];


emptyQ[l_List] := Length[l] == 0;


partition[expr_List, n_Integer] := With[{
		l = Length[expr],
		m = Partition[expr, n]
	},
	If[Mod[l, n] == 0,
		m,
		m~Join~{expr[[l - Mod[l, n] + 1;;]]}
	]
];


(* ::Subsection:: *)
(*Script utilities*)


SetAttributes[silent, HoldFirst];
silent[expr_] := Block[{Print}, expr];

ruleLogWrite = {
	"[info]" -> "\033[1;37m[info]\033[0m",
	"[ERROR]" -> "\033[1;31m[ERROR]\033[0m",
	"[time]" -> "\033[1;35m[time]\033[0m",
	"[seconds]" -> "\033[1;35m[seconds]\033[0m",
	"[test]" -> "\033[1;34m[test]\033[0m",
	"[OK]" -> "\033[1;32m[OK]\033[0m",
	"[note]" -> "\033[1;33m[note]\033[0m",
	"[running]" -> "\033[1;36m[running]\033[0m",
	"[exit]" -> "\033[1;36m[exit]\033[0m",
	"[warning]" -> "\033[0;33m[warning]\033[0m"
};


logwrite[message_] := If[$Notebooks,
	Print[message],
	WriteString["stderr", If[$Colorize, StringReplace[ToString@message, ruleLogWrite], ToString@message]]
];


SetAttributes[log, HoldRest];
Options[log] = {"endl" -> "\n", "prefix" -> "[info]: "};
log[expr_String, OptionsPattern[]] := With[{
		message = StringJoin[OptionValue["prefix"], expr, OptionValue["endl"]]
	},
	logwrite[message];
];
log[expr_, OptionsPattern[]] := With[{
		message = StringJoin[OptionValue["prefix"], ToString@InputForm[expr], OptionValue["endl"]]
	},
	logwrite[message];
];
log[message_, expr_, OptionsPattern[]] := (
	logwrite[StringJoin[OptionValue["prefix"], ToString[message]]];
	silent[expr];
	logwrite[OptionValue["endl"]];
);


SetAttributes[timing, HoldAll];
timing[message_, expr_] := Module[{time},
	log[StringPadRight[ToString@message <> " ", 60, "."], "prefix"-> "[time]: ", "endl" -> " ... "];
	time = First@Timing[silent[expr]];
	log[ToString@NumberForm[time, {6, 2}], "prefix" -> "", "endl" -> " [seconds]\n"];
	Return[time];
];
timing[expr_] := timing[HoldForm[expr], expr];


makeDirectory[path_] := (
	If[Not@FileExistsQ[path],
		log[ToString@StringForm["making directory '``' ... ", path]];
		CreateDirectory[path];
	];
	If[Not@DirectoryQ[path],
		log[ToString@StringForm["failed to make/find directory '``'", path], "prefix" -> "[ERROR]: "];
		exit[1];
	];
	log[ToString@StringForm["directory '``' does exist", AbsoluteFileName[path]]];
);


SetAttributes[check, HoldAll];
check[message_, expr_] := Module[{result},
	log[StringPadRight[ToString@message <> " ", 60, "."], "prefix"->"[test]: ", "endl" -> " ... "];
	result = ((expr) === True);
	log[If[result, "[OK]", "[ERROR]"], "prefix"->""];
	Return[result];
];
check[expr_] := check[HoldForm[expr], expr];


exit[code_:0] := (
	log["killing processes before exit ... ",
		 KillProcess /@ Processes[],
		 endl->"[OK]\n"
	];
	log["closing links before exit ... ",
		 LinkClose /@ Links[],
		 endl->"[OK]\n"
	];
	log[ToString@StringForm["[``]", DateString[]],
		prefix->"[exit]: "
	];
	Exit[code]
);


SetAttributes[note, HoldFirst];
note[expr_, func_] := With[{result = expr, hf=HoldForm[InputForm[expr]]},
	If[result =!= Null,
		With[{hr = func[result]},
      log[ToString@StringForm["`` = ``", hf, hr], "prefix"->"[note]: "]
    ],
		log[ToString@hf, "prefix"->"[note]: "]
	];
	Return[result];
];
note[expr_] := note[expr, InputForm];


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


error[expr_] := log[expr, prefix->"[ERROR]: "];

warning[expr_] := log[expr, prefix->"[warning]: "];

SetAttributes[llog, HoldRest];
llog[message_String, expr_] := With[{
    messageString = StringPadRight[message <> " ", 60, "."]
  },
  (
    log[messageString, endl->" ... "];
    silent@expr;
		log["[OK]", prefix->""];
  )
];
llog[message_, expr_] := With[{
    messageString = ToString@HoldForm[InputForm[message]]
  },
  llog[messageString, expr]
];

install[fname_String, path_String:"bin"] := With[{
    fnameWindows=FileNameJoin[{path, ToString@StringForm["``.exe", fname]}],
    fnameUnix=FileNameJoin[{path, fname}],
		fnameWSL=FileNameJoin[{path, ToString@StringForm["``.bin", fname]}]
  },
	Which[
	 ($OperatingSystem === "Unix" && FileExistsQ[fnameUnix]), (* plane Linux installation *)
   (
	   log[Evaluate["installing '" <> fnameUnix <>"' ... "]];
      Install[fnameUnix]
   ),
   ($OperatingSystem === "Windows" && FileExistsQ[fnameWindows]), (* plain Windows installation *)
	 (
	   log[Evaluate["installing '" <> fnameWindows <>"' ... "]];
		 Install[fnameWindows]
   ),
   (FileExistsQ[fnameWSL]), (* WSL on Windows installation *)
	 (
	   Abort[];
	   log[Evaluate["installing '" <> fnameWSL <>"' ... "]];
		 log[RunProcess[{fnameWSL, "-linkcreate", "-linkprotocol", "TCPIP"}]];
   ),
	 True, error[StringForm["can not find correct file for ``", $OperatingSystem]];
  ]
];

(* ::Section:: *)
(*End*)


End[];


log[ToString@StringForm["[``] using ``", DateString[], "RG`Tools`"]];


EndPackage[];


(* Local Variables: *)
(* mode: wl *)
(* compile-command: "math -script RG/SyntaxChecker/check.wl *.wl" *)
(* End: *)
