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


processList::usage = "processList[fs][expr] apply list of functions to expression, returns results of all steps"


process::usage = "process[fs][expr] apply list of functions to expression, returns {expr, final result}"


partition::usage = "partition[list, n] \[LongDash] partition list to n onoverlapping sublists of length n, and (if necessary) appends the rest elements"


ffirst::usage = "ffirst[list] return first element of flattened list
ffirst[list, verbose -> False] suppress warnings"


flast::usage = "flast[list] return last element of flattened list
flast[list, verbose -> False] suppress warnings"


emptyQ::usage = "emptyQ[list] returns True for empty list {}"


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Kernel working and logging*)


reset[] := (
	print[StringForm["[``]: closing wolfram session ...", DateString[]]];
	Quit[];
);


SetAttributes[print, {HoldFirst}];
Options[print] = {
	"verbose"->False,
	"stream"->"stdout",
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
	pprint[Context[expr]];
	info[expr];
	If[Not[emptyQ[UpValues[expr]]], pprint[UpValues[expr], Column, verbose->False]];
	If[Not[emptyQ[OwnValues[expr]]], pprint[OwnValues[expr], Column, verbose->False]];
	If[Not[emptyQ[DownValues[expr]]], pprint[DownValues[expr], Column, verbose->False]];
);
info[expr_String] := (
	print[Names[expr] // partition[#, 4]&, Column];
);


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*Equations*)


SetAttributes[present, HoldFirst];
present[expr_] := HoldForm[expr] == expr;


processList::duplicates = "processList contains unused functions"


processList[fs_List][expr_] := With[
	{result = FoldList[#2[#1]&, expr, fs]},
	If[Not @ DuplicateFreeQ[result], Message[processList::duplicates]];
	result
];
processList[fs__][expr_] := processList[{fs}][expr];


process[fs_List][expr_] := {expr, (RightComposition@@fs)[expr]};
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


(* ::Subsection::Closed:: *)
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


(* ::Section:: *)
(*End*)


End[];


EndPackage[];
