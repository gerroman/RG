(* ::Package:: *)

(* ::Section:: *)
(*BaseUtils*)


BeginPackage["RG`BaseUtils`", {"RG`Tools`"}];


off::usage = "off[message, expr] evaluate expression with the message temporally off";
on::usage = "on[message, expr] evaluate expression with the message temporally on";


hold::usage = "hold[pattern] apply Hold to matches of the `pattern`
hold[{x1,...}] apply Hold to specific xs";

holdform::usage = "holdform[pattern] apply HoldForm to matches of the `pattern`
holdform[{x1,...}] apply HoldForm to specific xs";


pass::usage = "pass[expr, other] evaluate first expr, returns nothing; use for mute tagged/untagged";


pullFactors::usage = "pullFactors[pattern, func] pull factors x from func which match func[___, (x:pattern) * ___, ___]
pullFactors[{x1,...}, func] pull concrete xs";


fixedPoint::usage = "fixedPoint[func] is shortcut for FixedPoint[#, func] &";


release::usage = "release[expr] apply ReleaseHold repeatedly";


factorItFast::usage = "factorItFast[x, func] faster version of factorIt";


powersPattern::usage = "powersPattern[{x1, ...}] return patterns for all possible powers of xs";




toRules::usage = "toRules shortcut for replacing equations to rules";


toEquals::usage = "toEquals shortcut for replacing rules to equations";


powerExpand::usage = "powerExpand shortcut for PowerExpand, but it leaves Logs unchanged";


collectLogs::usage = "collectLogs reduce sums of logarithms assuming real positive arguments";


changeLogPower::usage = "changeLogPower[power] change arguments of logarithms exponenting them to the power, assuming real positive arguments";


complexToAbs::usage = "complexToAbs replace products expr * Conjugate[expr] to Abs[expr]";


solve::usage = "solve[eqs_, vars_] solve equations eqs w.r.t. vars";


jacobian::usage = "jacobian[xs, ys, rules] return jacobian matrix, J,in terms of new variables, ys, for variable transitions xs -> ys, with transitions rules of type x[i] -> f[i][ys],
J[[i, j]] = D[y[i], x[j]] in terms of ys";


changeVars::usage = "changeVars[xs, ys, rules][expr] changes variables in expr, including in differential operators";


push::usage = "push[outer, inner][expr] pushes the outer function through the inner function";
pushRule::usage = "pushRule[outer, inner] creates a rule to push outer function into the inner function";


Begin["`Private`"];


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
			Thread[Thread[Hold[xs]] -> Thread[Hold[xs]]],
			Thread[-xs -> -Thread[Hold[xs]]],
			Thread[xs -> Thread[Hold[xs]]]
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

SetAttributes[holdform, HoldAll];
holdform[xs_List] := With[{
		rule = Join[
			Thread[Thread[HoldForm[xs]] -> Thread[HoldForm[xs]]],
			Thread[-xs -> -Thread[HoldForm[xs]]],
			Thread[xs -> Thread[HoldForm[xs]]]
		]
	},
	ReplaceRepeated[#, rule]&
];
holdform[pattern_] := Function[expr,
	With[{xs = Union@Cases[{expr}, pattern, Infinity]},
		holdform[xs][expr]
	]
];
holdform[xs__] := holdform[{xs}];


SetAttributes[pass, HoldAll];
pass[expr_, ___] := (expr;);


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


fixedPoint[func_, args___] := FixedPoint[func, #, args]&;


release = fixedPoint[ReleaseHold];


factorize[expr_, x_] := With[{xn = -x}, With[
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


powersPattern[xs_List] := (Subsets[xs] // Reverse //
	Map[#^_. &, #, {2}] & //
	Apply[Times, #, {1}] & //
	PowerExpand // ReplaceAll[x_ y_Optional :> y]
);


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


Options[solve] = {All -> False};
solve[eqs_, vars_, opts:OptionsPattern[]] := Block[{var`solve},
	With[{xs = Array[var`solve, Length[Flatten[{vars}]]]},
		With[{rule = Thread[vars -> xs]},
			With[{sol = Solve[eqs /. rule, xs] // ReplaceAll[Reverse /@ rule]},
				If[!OptionValue[All],
					(
						Assert[Length[sol] == 1];
						First[sol]
					),
					sol
				]
			]
		]
	]
];
solve[vars_] := solve[#, vars]&;
solve[vars_, All->all_] := solve[#, vars, All->all]&;


pushRule[outer_Function, inner_Function] := outer[inner[expr_]] :> inner[outer[expr]];
pushRule[outer_Function, inner_List] := Map[pushRule[outer, #]&, inner];
pushRule[outer_Function, inner__] := pushRule[outer, {inner}];
push[outer_, inner__] := ReplaceRepeated[#, pushRule[outer, inner]]&;


jacobian[xs_List, ys_List, rules_List:{}] := Inverse[Outer[D, xs /. rules, ys]];


changeVars[xs_List, ys_List, rules_List:{}][expr_] := With[{
		jcbn = jacobian[xs, ys, rules],
		n = Length[xs]
	},
	expr //	ReplaceAll[{Derivative -> Hold[Derivative]}] // ReplaceRepeated[#, {
			(head:Hold[Derivative][orders__ /; Total[{orders}] == 1])[f_][Sequence @@ xs] :> With[{
					pos = FirstPosition[head, 1] // First
				},
				Sum[(
						Derivative[Sequence @@ SparseArray[{k -> 1}, {n}]][f][Sequence @@ ys] *
						jcbn[[k, pos]]
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
							jcbn[[k, pos]]
						),
						{k, 1, n}
					]
				]
			]
		}]& // ReplaceAll[f_[Sequence @@ xs] -> f[Sequence @@ ys]] // ReplaceAll[rules] // release // Expand
];


End[];


EndPackage[];


Print[ToString@StringForm["[info]: '``' loaded", FileNameTake[$InputFileName, -3]]];

