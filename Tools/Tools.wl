(* ::Package:: *)

(* ::Section:: *)
(*Tools*)


BeginPackage["RG`Tools`"];


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

funion::usage = "funion[expr] return union of elements in the flattened sublists of"

emptyQ::usage = "emptyQ[list] returns True for empty list {}"

cases::usage = "cases[pattern] \[LongDash] function to get union expressions matching 'pattern'
cases[expr_, patter_] call cases[pattern][expr]
"

makeDirectory::usage = "makeDirectory[path] \[LongDash] creates 'path' if it does note exists"

silent::usage = "silent[expr] \[LongDash] evaluates 'expr' with turned off Print"

log::usage = "log[expr] \[LongDash] converts 'expr' to string and write an info message to 'stderr'"

timing::usage = "timing[expr] \[LongDash] timing 'expr' and write an info message to 'stderr'"

check::usage = "check[expr] \[LongDash] check 'expr' to be True, write an info message to 'stderr'"

exit::usage = "exit[code] \[LongDash] exit with code from script
exit[] \[LongDash] call exit[0]"

note::usage = "note[expr] \[LongDash] converts 'expr' (HoldForm input and InputForm output) to strings and write an info message to 'stderr'"


sizeOf::usage = "sizeOf[expr] \[LongDash] evaluates number of leafs and size in bytes of 'expr'"

$Colorize::usage = "$Colorize = True to colorize output (default: 'False' for Windows, 'True' for Linux)"

setPostProcessing::usage = "setPostProcessing[True] to duplicate In[]/Out[] to stderr
setPostProcessing[False] to disable feature";

install::usage = "install[fname] \[LongDash] install 'fname.ext' adding correct extension from 'bin' directory
install[fname, path] \[LongDash] install 'fname.ext' adding correct extension using 'path' as directory
"

error::usage = "error[expr] \[LongDash] log  'expr' with '[ERROR]' prefix"

warning::usage = "warning[expr] \[LongDash] log  'expr' with '[warning]' prefix"

llog::usage = "llog[message_, expr_] \[LongDash] prints 'message', call 'expr', print '[OK]' at the end, return expr"
llog::usage = "llog[expr_] \[LongDash] prints 'expr', call 'expr', print '[OK]' at the end, return expr"


checkExists::usage = "checkExists[fname] \[LongDash] check if file exists and print messages to log return True/False
checkExists[{fname1,...}] \[LongDash] check if all files in list"


argparse::usage = "argparse[] \[LongDash] returns {argc, argv}"


timeString::usage = "timeString[] form a time string";
timeStamp::usage = "timeStamp[] \[LongDash] print timeString[]";
systemString::usage = "timeString[] form a system string";
systemStamp::usage = "systemStamp[] \[LongDash] print systemString[]";


head::usage = "head[fname] \[LongDash] return first line of the text file";


$MessageLength::usage = "$messageLength (default = 80)";
$LongMessageFactor::usage = "$LongMessageFactor (default = 10)";

echo::usage = "echo[expr] \[LongDash] prints and return expr"

TeXString::usage = "TeXString[expr] format ``expr'' as TeX-string
";


TeXPrint::usage = "TeXPrint[expr] print ``expr'' surrounded by \\begin{equation}, \\end{equation}
	TeXPrint[expr, tag] print ``expr'' surrounded by \\begin{equation}\\label{tag}, \\end{equation}
";


stringForm::usage = "stringForm[expr] formats string from ``expr''";
stringTrim::usage = "stringTrim[string] trim long ``string'' to $MessageLength";

(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Kernel working and logging*)


$Colorize = ($OperatingSystem === "Unix" && Not[$Notebooks]);

setPostProcessing[bool:(True|False)] := If[bool,
	(
		$Post = Function[expr,
			If[expr =!= Null,
				WriteString["stderr", StringForm["\rOut[`1`] = `2`\n",
					$Line,
					StringTrim[StringPadRight[stringForm[expr], $LongMessageFactor * $MessageLength]] <> " ... "]
				];
			];
			WriteString["stderr", StringForm["\rIn[`1`] := ", $Line + 1]];
			If[$Notebooks, expr, Null]
		];
	),
	Clear[$Post];
];
setPostProcessing[] := setPostProcessing[True];


$MessageLength = 60;
$LongMessageFactor = 10;


reset[exitcode_:0] := (
	log[StringForm["<``>", DateString[]], Null, "prefix"->"[exit]: ", "endl"->"\n\n"];
	Exit[exitcode];
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
	If[$Notebooks,
		Print[(expr // func)];
		Return[]
	];
	If[verbose, WriteString[stream, ToString@OptionValue["header"]]];
	WriteString[stream, (expr // func // ToString)];
	WriteString[stream, sep];
];


SetAttributes[pprint, {HoldFirst, Listable}];
pprint[expr_, opts:OptionsPattern[]] := pprint[expr, Identity, opts];
pprint[expr_, func_, opts:OptionsPattern[]] := print[HoldForm[expr] // rewriteIt[ReleaseHold, func], opts];


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
  If[ValueQ[expr::usage], print[expr::usage, "verbose"->True, "header"->"[usage]: "]];
	If[Not[emptyQ[Attributes[expr]]], pprint[Attributes[expr], Column, "verbose"->False]];
	If[Not[emptyQ[Options[expr]]], pprint[Options[expr], Column, "verbose"->False]];
);
info[expr_Symbol, All] := (
	info[expr];
	If[Not[emptyQ[UpValues[expr]]], pprint[UpValues[expr], Column, "verbose"->False]];
	If[Not[emptyQ[OwnValues[expr]]], pprint[OwnValues[expr], Column, "verbose"->False]];
	If[Not[emptyQ[DownValues[expr]]], pprint[DownValues[expr], Column, "verbose"->False]];
);
info[expr_String] := (
	print[Names[expr] // partition[#, 4]&, Column];
);


cases[expr_, pattern_] := Union@Cases[{expr}, pattern, Infinity];
cases[pattern_] := Function[expr, cases[expr, pattern]];



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


SetAttributes[present, {HoldFirst, Listable}];
present[expr_] := HoldForm[expr] == expr;


processList::unused = "function '``' has no effect in processList";
processList[fs_List][expr_] := With[{result = FoldList[#2[#1]&, expr, fs]},
	If[Not @ DuplicateFreeQ[result],
		Module[{prev, pos},
			pos = LengthWhile[result, With[{test = (# =!= prev)}, (prev=#;test)]&];
			log[ToString@StringForm[processList::unused, stringForm[fs[[pos]]]], "prefix"->"[ERROR]: "];
		];
	];
	result
];
processList[fs__][expr_] := processList[{fs}][expr];

Options[process] = {"verbose" -> False};
process[fs_List, opts:OptionsPattern[]][expr_] := If[OptionValue["verbose"],
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
ffirst[expr_List, OptionsPattern[]] := With[{flat = Flatten[expr]},
	If[OptionValue["verbose"] && Length[flat] > 1, Message[ffirst::argx, flat]];
	First[flat]
];


flast::argx = "List `1` contains contains more than one element"


Options[flast] = {"verbose" -> True};
flast[expr_List, OptionsPattern[]] := With[{flat = Flatten[expr]},
	If[OptionValue["verbose"] && Length[flat] > 1, Message[flast::argx, flat]];
	Last[flat]
];


funion[expr_List] := Union[Flatten[expr]];


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


SetAttributes[silent, HoldAll];
silent[expr_] := Block[{Print}, expr];

ruleLogWrite = {
	"[info]" -> "\033[1;37m[info]\033[0m",
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
};


logwrite[message_] := If[$Notebooks,
	Print[ToString@message],
	WriteString["stderr", If[$Colorize, StringReplace[ToString@message, ruleLogWrite], ToString@message]]
];


SetAttributes[log, HoldRest];
Options[log] = {
	"prefix" -> "[info]: ",
	"endl" :> If[$Notebooks, "", "\n"]
};
log[expr_String, OptionsPattern[]] := With[{
		message = StringJoin[OptionValue["prefix"], expr, OptionValue["endl"]]
	},
	logwrite[message];
];
log[expr_StringForm, OptionsPattern[]] := With[{
		message = StringJoin[OptionValue["prefix"], ToString[expr], OptionValue["endl"]]
	},
	logwrite[message];
];
log[expr_, OptionsPattern[]] := With[{
		message = StringJoin[OptionValue["prefix"], ToString[expr], OptionValue["endl"]]
	},
	logwrite[message];
];
log[message_, expr_, OptionsPattern[]] := (
	logwrite[StringJoin[OptionValue["prefix"], ToString[message]]];
	silent[expr];
	logwrite[OptionValue["endl"]];
);


SetAttributes[timing, HoldAll];
timing[message_, expr_] := Module[{time, result},
	log[StringPadRight[ToString@message <> " ", $MessageLength, "."], "prefix"-> "[time]: ", "endl" -> " ... \n"];
	time = First@AbsoluteTiming[result = silent[expr]];
	log[StringPadRight["", $MessageLength, "."], "prefix"-> "[time]: ", "endl" -> " ... " <> ToString@NumberForm[time, {6, 2}] <> " [seconds]\n"];
	Return[{time, result}];
];
timing[expr_] := timing[HoldForm[expr], expr];


makeDirectory[path_] := With[{fname = ToString[path]},
	If[Not@FileExistsQ[fname],
		log[StringForm["making directory '``' ... ", fname]];
		CreateDirectory[fname];
	];
	If[Not@DirectoryQ[fname],
		error[StringForm["can not create/find directory '``'", fname]];
		Return[$Failed];
	];
	Return[AbsoluteFileName[fname]]
];


SetAttributes[check, HoldFirst];
Options[check] = {Simplify->Identity};
check[expr_, message_String, opts:OptionsPattern[]] := Module[{flag, result, func=OptionValue[Simplify]},
	log[message, "prefix"->"[test]: ", "endl" -> " ... "];
	result = func[expr];
  flag = result === True;
	log[If[flag, "[OK]", "[ERROR]"], "prefix"->""];
	Return[result];
];
check[expr_, message_StringForm, opts:OptionsPattern[]] := check[ToString@messag
e, expr, opts];
check[expr_, opts:OptionsPattern[]] := check[expr, stringTrim[stringForm[expr]], opts];


exit[code_Integer] := If[Not[$Notebooks], (
		(* log["killing processes", KillProcess /@ Processes[], "prefix"->"[exit]: ", "endl" -> "\n"]; *)
		(* log["closing links", LinkClose /@ Links[], "prefix"->"[exit]: ", "endl" -> "\n"]; *)
		log[ToString@StringForm["<``>", DateString[]], Null, "prefix"->"[exit]: ", "endl"->"\n\n"];
		Exit[code];
	)
];
exit[bool:(True|False)] := exit[If[bool, 0, 1]];
exit[] := exit[0];


SetAttributes[note, HoldFirst];
Options[note] = {
	"prefix" -> "[note]: ",
	"endl" -> "\n"
};
note[expr_, func_, opts:OptionsPattern[]] := (
	log[stringForm[expr], "prefix"->OptionValue["prefix"], "endl"->""];
	With[{result = expr},
		If[result =!= Null, With[{string=func[result]},
			  log[stringForm[result], "prefix"->" = ", "endl"->OptionValue["endl"]]
      ],
			log["Null", "prefix"->" = ", "endl"->OptionValue["endl"]]
		];
		result
	]
);
note[expr_, opts:OptionsPattern[]] := note[expr, Identity, opts];


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


error[expr_, opts:OptionsPattern[]] := log[expr, "prefix"->"[ERROR]: ", opts];
warning[expr_, opts:OptionsPattern[]] := log[expr, "prefix"->"[warning]: ", opts];

SetAttributes[llog, HoldAll];
Options[llog] = {
	"prefix" -> "[....]: ",
	"endl" -> "\n[info]: complete\n"
};
llog[message_String, expr_, opts:OptionsPattern[]] := Module[{
		messageString = StringPadRight[message <> " ", $MessageLength, "."],
		prefix = OptionValue["prefix"],
		endl = OptionValue["endl"],
		result
	},
	log[messageString, "prefix"->prefix, "endl" -> " ... "];
	result = silent@expr;
	log["", "prefix"->"", "endl" ->endl];
	Return[result];
];
llog[message_StringForm, expr_, opts:OptionsPattern[]] := llog[ToString@message, expr, opts];
llog[expr_, opts:OptionsPattern[]] := With[{messageString = stringForm[expr]},
	llog[messageString, expr, opts]
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



Options[checkExists] = {"verbose" -> True};
checkExists[results_List, opts:OptionsPattern[]] := With[{
		existance = Map[(# === True)&, FileExistsQ /@ results]
	},
	Which[
		OptionValue["verbose"] && (And @@ existance), (
			log[StringForm["'``' does exist", #], "prefix"->"[....]: "]& /@ results;
			log["", "prefix"->"[RESULT]: ", "endl"->""];
			Write["stdout", results];
		),
		Not[And@@existance], (
			log[StringForm["'``' does not exist", #], "prefix"->"[warning]: "]& /@ Pick[results, Not/@existance];
		)
	];
	Return[(And @@ existance)];
];
checkExists[result_, opts:OptionsPattern[]] := checkExists[{result}, opts];


argparse[] := Which[
	$ScriptCommandLine =!= {}, {Length[#], #}&[$ScriptCommandLine],
	Length[$CommandLine] >= 2 && $CommandLine[[2]] === "-script", {Length[#], #}&[$CommandLine[[3;;]]],
	True, {0, {}}
];


argparse[name_String, False] := With[{argv = Last[argparse[]]},
	With[{value = MemberQ[argv, "-" <> name]},
		If[value,
			log[StringForm["using `` = `` (command line flag)", name, value], "prefix"->"[args]: "],
			log[StringForm["using `` = `` (absent command line flag)", name, value], "prefix"->"[args]: "]
		];
		value
	]
];

argparse[name_String, default_Integer] := Module[
	{argc, argv, pos, value},
	{argc, argv} = argparse[];
	pos = Position[argv, "-" <> name];
	If[Length[pos] != 1 || pos[[1, 1]] + 1 > argc,
		log[StringForm["using `` = `` (default value)", name, default], "prefix"->"[args]: "];
		Return[default]
	];
	value = ToExpression[argv[[pos[[1, 1]] + 1]]];
	If[Not@IntegerQ[value],
		log[StringForm["using `` = `` (default value)", name, default], "prefix"->"[args]: "];
		Return[default]
	];
	log[StringForm["using `` = `` (command line argument, default = ``)", name, value, default], "prefix"->"[args]: "];
	Return[value]
];

argparse[name_String, default_Real] := Module[
	{argc, argv, pos, value},
	{argc, argv} = argparse[];
	pos = Position[argv, "-" <> name];
	If[Length[pos] != 1 || pos[[1, 1]] + 1 > argc,
		log[StringForm["using `` = `` (default value)", name, default], "prefix"->"[args]: "];
		Return[default]
	];
	value = ToExpression[argv[[pos[[1, 1]] + 1]]];
	If[Not@RealQ[value],
		log[StringForm["using `` = `` (default value)", name, default], "prefix"->"[args]: "];
		Return[default]
	];
	log[StringForm["using `` = `` (command line argument, default value = ``)", name, 1.0 * value, default], "prefix"->"[args]: "];
	Return[1.0 * value]
];

argparse[name_String, default_String] := Module[
	{argc, argv, pos, value},
	{argc, argv} = argparse[];
	pos = Position[argv, "-" <> name];
	If[Length[pos] != 1 || pos[[1, 1]] + 1 > argc,
		log[StringForm["using `` = '``' (default value)", name, default], "prefix"->"[args]: "];
		Return[default]
	];
	value = argv[[pos[[1, 1]] + 1]];
	log[StringForm["using `` = '``' (command line argument, default='``')", name, value, default], "prefix"->"[args]: "];
	Return[value]
];


timeString[] := DateString[{"<", "Year", "-", "Month", "-", "Day", " ", "Hour",":", "Minute", ":", "Second", ">"}];
timeStamp[] := With[{stamp = timeString[]},
	If[$Notebooks, Print[stamp]];
	log[stamp, prefix-> "[date]: "];
];

systemString[] := ToString@StringForm["``@`` : Wolfram Mathematica ``", $UserName, $MachineName, $Version];
systemStamp[] := With[{stamp = systemString[]},
	If[$Notebooks, Print[stamp]];
	log[stamp];
];


head[fname_String] := Module[{stream, result = $Failed},
	If[FileExistsQ[fname],
		(
      log[StringForm["\"``\" (`` kB)", fname, 1.`3*10^(-3) * FileByteCount[fname]], "prefix"->"[file]: "];
			stream = OpenRead[fname];
			result = ReadLine[stream];
			Close[stream];
		),
		error[StringForm["can not find '``'", fname]];
	];
	Return[result];
];
head[fname_String, n_Integer] := Module[{stream, result = $Failed},
	If[FileExistsQ[fname],
		(
      log[StringForm["\"``\" (`` kB)", fname, 1.`3*10^(-3) * FileByteCount[fname]], "prefix"->"[file]: "];
			stream = OpenRead[fname];
			result = StringRiffle[
				Table[ReadLine[stream], n] // DeleteCases[EndOfFile],
				{"", "\n", ""}
			];
			Close[stream];
		),
		error[StringForm["can not find '``'", fname]];
	];
	Return[result];
];

echo[expr_] := (
	log[stringForm[expr], "prefix"->"[echo]: "];
	expr
);


TeXString = Function[expr, expr // TeXForm // ToString];

SetAttributes[TeXPrintStream, HoldFirst];
TeXPrintStream[expr_, stream_] := (
	WriteString[stream, #]& /@ {
		"\\begin{equation}\n",
		TeXForm[expr],
		"\n\\end{equation}\n"
	}
);
TeXPrintStream[expr_, tag_String, stream_] := (
	WriteString[stream, #]& /@ {
		"\\begin{equation}\n",
		StringForm["\\label{``}\n", tag],
		TeXForm[expr],
		"\n\\end{equation}\n"
	}
);

SetAttributes[TeXPrint, HoldFirst];
Options[TeXPrint] = {
	"stream"->"stdout"
};

TeXPrint[expr_, opts:OptionsPattern[]] := With[{stream=OptionValue["stream"]},
	If[stream === "stdout" || stream === "stderr",
		TeXPrintStream[expr, stream],
		(
			log[StringForm["TeXPrint[] to '``'", stream]];
			With[{f = OpenWrite[stream]},
				TeXPrintStream[expr, f];
				Close[f];
			];
		)
	];
];

TeXPrint[expr_, tag_String, opts:OptionsPattern[]] := With[{stream=OptionValue["stream"]},
	If[stream === "stdout" || stream === "stderr",
		TeXPrintStream[expr, tag, stream],
		(
			log[StringForm["TeXPrint[] to '``'", stream]];
			With[{f = OpenWrite[stream]},
				TeXPrintStream[expr, tag, f];
				Close[f];
			];
		)
	];
];


SetAttributes[stringForm, HoldAll]
stringForm[expr_] := StringTake[ToString[InputForm[Unevaluated[expr]]], {13, -2}];

stringTrim[expr_String] := StringTrim[StringPadRight[expr, $MessageLength, "."]];


(* ::Section:: *)
(*End*)


systemStamp[];
timeStamp[];

path`tmp = makeDirectory[FileNameJoin[{$TemporaryDirectory, $UserName}]];
path`run = FileNameJoin[{path`tmp, "run"}];
path`figs = FileNameJoin[{path`tmp, "figs"}];

With[{fname = FindFile["src/init.wl"]},
	If[fname =!= $Failed,
		SetDirectory[ParentDirectory[DirectoryName[fname]]];
		path`run = FileNameJoin[{Directory[], "run"}];
		path`figs = FileNameJoin[{Directory[], "figs"}];
	];
];
path`cwd = Directory[];


note[path`cwd];

note[path`run];

note[path`figs];


If[Environment["$MATHEMATICA_LAUNCH_KERNELS"] =!= $Failed,
	Quiet[
		LaunchKernels[];
		log[StringForm["$KernelCount = ``", $KernelCount]];
	];
];

If[Environment["$MATHEMATICA_POST_PROCESSING"] =!= $Failed,
	setPostProcessing[True]
];


End[];


EndPackage[];
