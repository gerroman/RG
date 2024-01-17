(* ::Package:: *)

BeginPackage["RG`BaseUtils`"];


(* ::Text:: *)
(*Common options: update, verbose.*)


update::usage = "
	update -> True option to force call definitions and save to file in load[]
";
verbose::usage = "
	verbose -> True make function more verbose
";


(* ::Text:: *)
(*Load definitions and figures from files*)


load::usage = "
	load[file, symbol, definitions] load from file (if it exists) or
	execute symbol's definitions and save it to file
";

loadFigure::usage = "
	loadFigure[fname, expr] load figure from file if it exists or execute expr and save figure to the file
";


(* ::Text:: *)
(*turn off/on messages during evaluating an expression*)


off::usage = "
	off[message, expr] evaluate expression with the message temporally off
";
on::usage = "
	on[message, expr] evaluate expression with the message temporally on
";


hold::usage = "
	hold[pattern] apply HoldForm to matches of pattern
	hold[{x1,...}] apply HoldForm to specific xs
";


pass::usage = "
	pass[expr, other] evaluate first expr, returns nothing; use for mute tagged/untagged
";


print::usage = "
	print[expr] evaluate first expr, print result
	print[expr, func] evaluate first expr, apply func, print result
";
modify::usage = "
	modify[pattern, func] create function to replace all matches of the pattern to results of application of the function func to these matches
	modify[{x1, ...}, func] create function for specific x1, ...
";


pullFactors::usage = "
	pullFactors[pattern, func] pull factors x from func which match func[___, (x:pattern) * ___, ___]
	pullFactors[{x1,...}, func] pull concrete xs
";


groupIt::usage = "
	groupIt[expr, func] find and group expr modified by functions func
";


fixedPoint::usage = "
	fixedPoint[func] is shortcut for FixedPoint[#, func] &
";


release::usage = "
	release[expr] apply ReleaseHold repeatedly
";


factorIt::usage = "
	factorIt[pattern, func] factor out factors matches pattern from func
	factorIt[{x1, ...}, func] factor out concrete xs
";

factorItFast::usage = "
	factorItFast[x, func] faster version of factorIt
";


pullIt::usage = "
	pullIt[pattern] pull out factors matches pattern
	pullIt[{x1, ...}, func] pull out concrete xs factors from all func arguments
";


powersPattern::usage = "
	powersPattern[{x1, ...}] return patterns for all possible powers of xs
";


changeSign::usage = "
	changeSign[x] change sign of x
";


rewriteIt::usage = "
	rewriteIt[func] rewrite equation (or rule, or expression) using func for the right hand side
	rewriteIt[funcL, funcR] rewrite equation (or rule, or expression) using funcL for the left hand side \
and funcR for the right hand side
";


toRules::usage = "
	toRules shortcut for replacing equations to rules
";

toEquals::usage = "
	toEquals shortcut for replacing rules to equations
";


powerExpand::usage = "
	powerExpand shortcut for PowerExpand, but it leaves Logs unchanged
";
collectLogs::usage = "
	collectLogs reduce sums of logarithms assuming real positive arguments
";
changeLogPower::usage = "
	changeLogPower[power] change arguments of logarithms exponenting them to the power,\
assuming real positive arguments
";

complexToAbs::usage = "
	complexToAbs replace products expr * Conjugate[expr] to Abs[expr]
";


solve::usage = "
	solve[eqs_, vars_] solve equations eqs w.r.t. vars
";


processList::usage = "
	processList[fs][expr] apply list of functions to expression return results of all steps
";
process::usage = "
	process[fs][expr] apply list of functions returns a list {expr, result}
";

ffirst::usage = "
	ffirst[list] return first element of flattened list
	ffirst[list, verbose -> False] suppress warnings
";
flast::usage = "
	flast[list] return last element of flattened list
	flast[list, verbose -> False] suppress warnings
";
jacobian::usage = "
	jacobian[xs, ys, rules] return jacobian matrix, J,in terms of new variables, ys, for variable transitions xs -> ys, with transitions rules of type x[i] -> f[i][ys],
	J[[i, j]] = D[y[i], x[j]] in terms of ys
";
changeVars::usage = "
	changeVars[xs, ys, rules][expr] changes variables in expr, including in differential operators
";
push::usage = "
	push[outer, inner][expr] pushes the outer function through the inner function
";


clearScreen::usage = "
	clearScreen[] \[LongDash] call Print for 50 empty lines
";
reset::usage = "
	reset[] \[LongDash] call clearScreen[], then Quit[];
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
show::usage = "
	show[expr] \[LongDash] forms an equation: HoldForm[expr] == expr
";

partition::usage = "partition[l, n] \[LongDash] partitions list to n
onoverlapping sublists of length n, and (if necessary) appends the rest elements";


Begin["`Private`"];


Protect[verbose];
Protect[update];

SetAttributes[load, HoldAll];
Options[load] = {update -> False, verbose -> False};
load::get = "[info]: load `1` ...";
load::save = "[info]: save `1` ...";
load::failed = "[error]: failed to load file `1`";
load[fname_String, symbol_Symbol, expr___, OptionsPattern[]] := With[{
		path = FileNameJoin[{Global`temporarydirectory, fname}]
	},
	If[FileExistsQ[path] && Not[OptionValue[update]], (
			If[OptionValue[verbose],
				Print[ToString[StringForm[load::get, path]]];
			];
			Get[path]
		), (
			expr;
			If[OptionValue[verbose],
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
			If[OptionValue[verbose],
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
Options[loadFigure] = {update -> False, verbose -> True, "force"->False};
loadFigure[fname_String, expr_, OptionsPattern[]] := With[{
		path = FileNameJoin[{Global`figuredirectory, fname}],
		hash = Hash[Hold[expr]],
		hashpath = FileNameJoin[{Global`figuredirectory, fname}] <> ".hash",
		force = OptionValue["force"]
	},
	If[(Not@FileExistsQ[path]
			|| Not@FileExistsQ[hashpath]
			|| (hash =!= Get[hashpath] && OptionValue[update])), (
			If[OptionValue[verbose],
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
			If[OptionValue[verbose],
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


SetAttributes[off, HoldAll];
off[message__, expr_] := (
	Off[message];
	With[{result = expr},
		On[message];
		result
	]
);

SetAttributes[on, HoldAll];
on[message__, expr_] := (
	On[message];
	With[{result = expr},
		Off[message];
		result
	]
);


SetAttributes[hold, HoldAll];
hold[xs_List] := With[{
		rule = Join[
			Thread[Thread[HoldForm[xs]] -> Thread[HoldForm[xs]]],
			Thread[-xs -> -Thread[HoldForm[xs]]],
			Thread[xs -> Thread[HoldForm[xs]]]
		]
	},
	ReplaceRepeated[#, rule]&
];
hold[pattern_] := Function[expr,
	With[{xs = Union@Cases[{expr}, pattern, Infinity]},
		hold[xs][expr]
	]
];
hold[xs__] := hold[{xs}];


SetAttributes[pass, HoldAll];
pass[expr_, ___] := (expr;);


SetAttributes[print, HoldFirst];
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


pullFactors[{}, func_, maxIter_:$IterationLimit] = Identity;
pullFactors[xs_List, func_, maxIter_:$IterationLimit] := With[
	{pattern = If[Length[xs] == 1, First@xs, Alternatives@@xs]},
	Function[expr,
		FixedPoint[ReplaceAll[func[a___, (x:pattern) * b_, c___] :> x func[a, b, c]],  expr, maxIter]
	]
];
pullFactors[pattern_, func_, maxIter_:$IterationLimit] := Function[expr,
	With[{xs = Union@Cases[{expr}, pattern, Infinity]},
		pullFactors[xs, func, maxIter][expr]
	]
];

(* pullFactors[arg_, func_, maxIter_:$IterationLimit] := With[{ *)
(*		pattern = If[Head[arg] === List, *)
(*			If[Length[arg] == 1, *)
(*				First@arg, *)
(*				Alternatives@@arg *)
(*			], *)
(*			arg *)
(*		] *)
(*   }, *)
(*   Function[{expr}, *)
(*	FixedPoint[ *)
(*		ReplaceAll[func[a___, (x:pattern) * b_, c___] :> x func[a, b, c]], *)
(*		expr, *)
(*		maxIter *)
(*	] *)
(*   ] *)
(* ]; *)


fixedPoint[func_, args___] := FixedPoint[func, #, args]&;


release = fixedPoint[ReleaseHold];


groupIt[xs_List, func_:Expand] := With[
	{rules = Map[x \[Function] ((x // modify[{x}, func]) -> x), xs]},
	ReplaceRepeated[#, rules] &
];
groupIt[x_, func_:Expand] := groupIt[{x}, func];


(* NOTE: strightforward matching can be long *)
factorIt[xs_List, func_:Plus] := With[{
	rules = Flatten[
			Map[
				x \[Function] With[
					{
						p = x,
						pn = -x
					}
					,
					{
						func[p*(a_.), p*(b_.)] :> p * func[a, b]
						, func[p*(a_.), pn*(b_.)] :> p * func[a, -b]
						, func[pn*(a_.), pn*(b_.)] :> pn * func[a, b]
					}
				]
				,
				xs
			]
		]
	},
	ReplaceRepeated[#, rules]&
];


factorIt[pattern_, func_:Plus] := Function[expr,
	With[{xs = Union@Cases[{expr}, pattern, Infinity]},
		factorIt[xs, func][expr]
	]
];


factorize[expr_, x_] := With[{xn = -x}, Module[
	{
		split = List @@ expr // changeSign[xn] //
					(SortBy[#, MatchQ[(x * _.)|(xn * _.)]]&) //
					(SplitBy[#, MatchQ[(x * _.)|(xn * _.)]]&),
		func = Head[expr]
	},
	Which[
	(Length[split] == 2) && (Length[Last[split]] > 1), (
		func[func @@ (First[split]), x * (func @@ (Last[split] / x))]
	),
	(Length[split] == 1) && MatchQ[First[First[split]], x * _.], (
		x * (func @@ (First[split] / x))
	),
	True, (
		expr
	)
	]
]];

factorItFast[l_List, levelspec_:{0}, func_:Plus] := (
	RightComposition @@ Map[factorItFast[#, levelspec, func]&, l]
);

factorItFast[x_, levelspec_:{0}, func_:Plus] := With[{
		rule = (expr_func) :> factorize[expr, x]
	},
	Replace[#, rule, levelspec]&
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


powersPattern[xs_List] := (Subsets[xs] // Reverse //
	Map[#^_. &, #, {2}] & //
	Apply[Times, #, {1}] & //
	PowerExpand // ReplaceAll[x_ y_Optional :> y]
);



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



toRules = ReplaceAll[#, Equal -> Rule] &;
toEquals = ReplaceAll[#, Rule -> Equal] &;


powerExpand = modify[_Power, PowerExpand];
collectLogs = With[{
	rules = {
		a_. Log[x_] + a_. Log[y_] :> a Log[x y]
		, a_. Log[x_] + b_. Log[y_] :> a Log[x/y] /; a == (-b)
	}},
	ReplaceRepeated[#, rules] &
];
changeLogPower[power_] := ReplaceAll[#, Log[x_] :> (1/power) * Log[x^power]] &;


complexToAbs[pattern_] = (ReplaceAll[#,
	{
		product:(expr_ * Conjugate[expr_]) :> Abs[expr]^2 /; MatchQ[expr, pattern]
	}
]&);


solve[eqs_, vars_] := Block[{var`solve},
	 With[{xs = Array[var`solve, Length[Flatten[{vars}]]]},
		With[{rule = Thread[vars -> xs]},

	 With[{sol =
		Solve[eqs /. rule, xs] // ReplaceAll[Reverse /@ rule]},
				Assert[Length[sol] == 1];
				First[sol]
			]
		 ]
		]
	 ];
solve[vars_] := solve[#, vars]&;


pushRule[outer_Function, inner_Function] := outer[inner[expr_]] :> inner[outer[expr]];
pushRule[outer_Function, inner_List] := Map[pushRule[outer, #]&, inner];
pushRule[outer_Function, inner__] := pushRule[outer, {inner}];
push[outer_, inner__] := ReplaceRepeated[#, pushRule[outer, inner]]&;


processList::duplicates = "processList contains unused functions";
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


Options[ffirst] = {verbose -> True};
ffirst::warning = "List `1` contains contains more than one element";
ffirst[expr_List, OptionsPattern[]] := Block[{flat = Flatten[expr]},
	If[OptionValue[verbose] && Length[flat] > 1, Message[ffirst::warning, flat]];
	First[flat]
];


Options[flast] = {verbose -> True};
flast::warning = "List `1` contains contains more than one element";
flast[expr_List, OptionsPattern[]] := Block[{flat = Flatten[expr]},
	If[OptionValue[verbose] && Length[flat] > 1, Message[flast::warning, flat]];
	Last[flat]
];


jacobian[xs_List, ys_List, rules_List:{}] := Inverse[Outer[D, xs /. rules, ys]];


changeVars[xs_List, ys_List, rules_List:{}][expr_] := Module[{
		jacobian = jacobian[xs, ys, rules],
		n = Length[xs]
	},
	expr //	ReplaceAll[{Derivative -> Hold[Derivative]}] //
	ReplaceRepeated[#, {
			(head:Hold[Derivative][orders__ /; Total[{orders}] == 1])[f_][Sequence @@ xs] :> With[{
					pos = FirstPosition[head, 1] // First
				},
				Sum[(
						Derivative[Sequence @@ SparseArray[{k -> 1}, {n}]][f][Sequence @@ ys] *
						jacobian[[k, pos]]
					),
					{k, 1, n}
				]
			],
			(head:Hold[Derivative][orders__ /; Total[{orders}] > 1])[f_][Sequence @@ xs] :> With[{
					pos = FirstPosition[head, (x_ /; x >= 1)] // First
				},
				With[{newhead = MapAt[(# - 1)&, head, {pos}]},
					Sum[(
							Hold[D][newhead[f][Sequence @@ xs],	ys[[k]]] *
							jacobian[[k, pos]]
						),
						{k, 1, n}
					]
				]
			]
		}
	] & // ReplaceAll[f_[Sequence @@ xs] -> f[Sequence @@ ys]] // ReplaceAll[rules] // release // Expand
];


clearScreen[] := If[!$Notebooks, Do[Print[], 50]];

reset[] := (
	print[StringForm["[``]: close wolfram session ...", DateString[]]];
	Quit[];
);


SetAttributes[pprint, {HoldFirst}];
pprint[expr_, opts:OptionsPattern[]] := print[
	HoldForm[expr] // rewriteIt[ReleaseHold]
	, opts
];
pprint[expr_List, opts:OptionsPattern[]] := print[
 Thread[HoldForm[expr]] // rewriteIt[ReleaseHold] // Column
 , opts
];
pprint[expr_List, func_, opts:OptionsPattern[]] := print[
 Thread[HoldForm[expr]] // rewriteIt[ReleaseHold, func] // Column
 , opts
];
pprint[expr_, func_, opts:OptionsPattern[]] := print[
	HoldForm[expr] // rewriteIt[ReleaseHold, func]
	, opts
];


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


SetAttributes[show, HoldAll];
show[expr_] := HoldForm[expr] == expr;


partition[expr_List, n_Integer] := With[{
		l = Length[expr],
		m = Partition[expr, n]
	},
	If[Mod[l, n] == 0,
		m,
		m~Join~{expr[[l - Mod[l, n] + 1;;]]}
	]
];


End[];


(* Print[$Context]; *)


EndPackage[];
