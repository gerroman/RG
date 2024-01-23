(* ::Package:: *)


BeginPackage["RG`Tools`"];


verbose::usage = "
	verbose -> True \[LongDash] make function output more verbose
";

print::usage = "
	print[expr] evaluate first expr, print result
	print[expr, func] evaluate first expr, apply func, print result
";

pprint::usage = "
	pprint[expr] \[LongDash] call print[HoldForm[expr] == expr]
	pprint[expr, func] \[LongDash] call print[HoldForm[expr] == func[expr]]
	pprint[expr, func, opts] \[LongDash] call print[HoldForm[expr] == func[expr], opts]
";

info::usage = "
	info[func] \[LongDash] get information about func: context, usage, attributes, options
	info[func, All] \[LongDash] get full information about func including up/down values
";

rewriteIt::usage = "
	rewriteIt[func][expr] \[LongDash] rewrite expr (equation, rule) using func for the right hand side
	rewriteIt[funcL, funcR][expr] \[LongDash] rewrite expr (equation, rule) using funcL for the left hand side \
and funcR for the right hand side
";

reset::usage = "
	reset[] \[LongDash] terminate session
";

partition::usage = "
  partition[list, n] \[LongDash] partition list to n onoverlapping sublists of length n, and (if necessary) appends the rest elements
";

installFrontEnd::usage = "installFrontEnd[] \[LongDash] install auxiliary FrontEnd for wolfram scripts";

uninstallFrontEnd::usage = "uninstallFrontEnd[] \[LongDash] uninstall auxiliary FrontEnd wolfram scripts";

show::usage = "show[expr] \[LongDash] use auxiliary FrontEnd to present result of expr";


Begin["`Private`"];


Protect[verbose];


SetAttributes[print, {HoldFirst}];
Options[print] = {
	verbose->False,
	"stream"->"stdout",
	"header" :> DateString[{"(* ", "Year", "/", "Month", "/", "Day", " : ", "Hour",":", "Minute", ":", "Second", " *)\n"}],
	"sep"->"\n"
};
print[expr_, opts:OptionsPattern[]] := print[expr, Identity, opts];
print[expr_, func_:Identity, opts:OptionsPattern[]] := With[{
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


reset[] := (
	print[StringForm["[``]: close wolfram session ...", DateString[]]];
	Quit[];
);


emptyQ[l_List] := Length[l] == 0;
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


partition[expr_List, n_Integer] := With[{
		l = Length[expr],
		m = Partition[expr, n]
	},
	If[Mod[l, n] == 0,
		m,
		m~Join~{expr[[l - Mod[l, n] + 1;;]]}
	]
];


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


End[];


EndPackage[];
