BeginPackage["RG`Tools`", {"RG`Scripts`"}];


installFrontEnd::usage = "installFrontEnd[] \[LongDash] install auxiliary FrontEnd for wolfram scripts"
uninstallFrontEnd::usage = "uninstallFrontEnd[] \[LongDash] uninstall auxiliary FrontEnd wolfram scripts"
show::usage = "show[expr] \[LongDash] use auxiliary FrontEnd to present result of expr"


process::usage = "process[fs][expr] apply list of functions to expression, returns {expr, final result}"
processList::usage = "processList[fs][expr] apply list of functions to expression, returns results of all steps"
present::usage = "present[expr] \[LongDash] forms an equation: HoldForm[expr] == expr"
modify::usage = "modify[pattern, func] create function to replace all matches of the pattern to results of application of the function func to these matches
modify[{x1, ...}, func] create function for specific x1, ..."
emptyQ::usage = "emptyQ[list] returns True for empty list {}"
partition::usage = "partition[list, n] \[LongDash] partition list to n onoverlapping sublists of length n, and (if necessary) appends the rest elements"


ffirst::usage = "ffirst[list] return first element of flattened list
ffirst[list, verbose -> False] suppress warnings"
flast::usage = "flast[list] return last element of flattened list
flast[list, verbose -> False] suppress warnings"
funion::usage = "funion[expr] return union of elements in the flattened sublists of"
cases::usage = "cases[pattern] \[LongDash] function to get union expressions matching 'pattern'
cases[expr_, patter_] call cases[pattern][expr]"

TeXString::usage = "TeXString[expr] format ``expr'' as TeX-string";
TeXPrint::usage = "TeXPrint[expr] print ``expr'' surrounded by \\begin{equation}, \\end{equation}
	TeXPrint[expr, tag] print ``expr'' surrounded by \\begin{equation}\\label{tag}, \\end{equation}";


factorIt::usage = "factorIt[pattern, func] factor out factors matches pattern from func
factorIt[{x1, ...}, func] factor out concrete xs";

pullIt::usage = "pullIt[pattern] pull out factors matches pattern
pullIt[{x1, ...}, func] pull out concrete xs factors from all func arguments";

groupIt::usage = "groupIt[expr, func] find and group expr modified by functions func";

changeSign::usage = "changeSign[x] change sign of x";


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Section:: *)
(*FrontEnd operations from scripts*)


installFrontEnd[serverFlag_:False] := With[{link = System`UseFrontEndDump`$felink},
	If[Not@MemberQ[Links[], link], Developer`InstallFrontEnd["Server"->True]];
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


(* ::Section:: *)
(*Equations*)


SetAttributes[present, {HoldFirst, Listable}];
present[expr_] := HoldForm[expr] == expr;


processList::unused = "function '``' has no effect in processList";
processList[fs_List][expr_] := With[{result = FoldList[#2[#1]&, expr, fs]},
	If[Not @ DuplicateFreeQ[result],
		Module[{prev, pos},
			pos = LengthWhile[result, With[{test = (# =!= prev)}, (prev=#;test)]&];
			error[StringForm[processList::unused, stringForm[fs[[pos]]]]];
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


modify[xs_List, func_] := With[{rules = Thread[Rule[xs, Map[func, xs]]]},
	ReplaceAll[rules]
];
modify[pattern_, func_] := Function[expr,
	With[{xs = Union@Cases[{expr}, pattern, Infinity]},	modify[xs, func][expr]]
];


(* ::Section:: *)
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


cases[expr_, pattern_] := Union@Cases[{expr}, pattern, Infinity];
cases[pattern_] := Function[expr, cases[expr, pattern]];


emptyQ[l_List] := Length[l] == 0;


partition[expr_List, n_Integer] := With[{l = Length[expr], m = Partition[expr, n]},
	If[Mod[l, n] == 0, m,	m~Join~{expr[[l - Mod[l, n] + 1;;]]}]
];


(* ::Section:: *)
(*TeX*)
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


changeSign[xs_List] := With[{
		rules = Map[x \[Function] (x^(p_.) expr_. :> (-x)^p (-1)^p expr), xs]
	},
	ReplaceAll[rules]
];
changeSign[pattern_] := Function[{expr},
	With[{xs = Union@Cases[{expr}, pattern, Infinity] // Flatten},
		changeSign[xs][expr]
	]
];
changeSign[xs__] := changeSign[{xs}];


factorItRules[x_, head_:Plus, func_:Identity] := With[
	{p = x, pn = -x}
	,
	{
		head[ p*(a_.),  p*(b_.)] :>  p * func[head[a,  b]],
		head[ p*(a_.), pn*(b_.)] :>  p * func[head[a, -b]],
		head[pn*(a_.), pn*(b_.)] :> pn * func[head[a,  b]]
	}
];

(* [NOTE]: strightforward matching can be long *)
factorIt[xs_List, head_:Plus, func_:Identity] := With[{
		rules = Flatten@Map[x \[Function] factorItRules[x, head, func],	xs]
	},
	ReplaceRepeated[#, rules]&
];


factorIt[pattern_, head_:Plus, func_:Identity] := Function[
	expr
	,
	With[{xs = Union@Cases[{expr}, pattern, Infinity]},
		factorIt[xs, head, func][expr]
	]
];


pullIt[xs_List, func_:Plus] := With[{
	rules = (x \[Function] With[{p = x},
				(func[p * b_., a_] :> p Map[(# / p) &, func[p b, a]])
			]) /@ xs
	},
	ReplaceAll[#, rules]&
];

pullIt[pattern_, func_:Plus][expr_] := With[{
		xs = Union@Cases[{expr}, pattern, Infinity]
	},
	pullIt[xs, func][expr]
];


groupIt[xs_List, func_:Expand] := With[
	{rules = Map[(func[#] -> #)&, xs]},
	ReplaceRepeated[#, rules] &
];
groupIt[x_, func_:Expand] := groupIt[{x}, func];

groupIt[expr:(_Hold|_HoldForm)] := groupIt[{expr}, ReleaseHold];
groupIt[expr:(_Hold|_HoldForm), func] := groupIt[{expr}, ReleaseHold/*func];
groupIt[expr:{(_Hold|_HoldForm)..}] := groupIt[expr, ReleaseHold];
groupIt[expr:{(_Hold|_HoldForm)..}, func] := groupIt[expr, ReleaseHold /* func];




(* ::Section:: *)
(*End*)


End[];


EndPackage[];


Print[ToString@StringForm["[info]: '``' loaded", $InputFileName]];
