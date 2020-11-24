(* ::Package:: *)

(* ::Text:: *)
(*Functions to perform routine transformations*)


BeginPackage["RG`Calculation`"];


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


Begin["`Private`"];


modify[xs_List, func_] := With[{
    rules = Thread[Rule[xs, Map[func, xs]]]
  },
  ReplaceAll[rules]
];
modify[pattern_, func_] := Function[expr,
  With[{
      xs = Union@Cases[{expr}, pattern, Infinity]
    },
    modify[xs, func][expr]
  ]
];


pullFactors[arg_, func_, maxIter_:$IterationLimit] := With[{
    pattern = If[Head[arg] === List,
      If[Length[arg] == 1, First@arg, Alternatives@@arg],
      arg
    ]
  },
  Function[{expr},
    FixedPoint[
      ReplaceAll[func[a___, (x:pattern) * b_, c___] :> x func[a, b, c]],
      expr,
      maxIter
    ]
  ]
];


fixedPoint[func_, args___] := FixedPoint[func, #, args]&;


release = fixedPoint[ReleaseHold];


groupIt[xs_List, func_:Expand] := With[
  {rules = Map[x \[Function] ((x // modify[{x}, func]) -> x), xs]},
  ReplaceRepeated[#, rules] &
];
groupIt[x_] := groupIt[{x}];


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
  With[
	  {
		  xs = Union@Cases[{expr}, pattern, Infinity]
		},
  	factorIt[xs, func][expr]
	]
];


factorize[expr_, x_] := Module[
  {
	  split = List @@ expr //
	    (SortBy[#, MatchQ[x * _.]]&) //
   		(SplitBy[#, MatchQ[x * _.]]&)
		, func = Head[expr]
	},
	If[
	  (
		  (Length[split] == 2) &&
	    (Length[Last[split]] > 1)
		),
	  func[
  	  func @@ (First[split])
			, x * (func @@ (Last[split] / x))
		]
		,
		expr
	]
];

factorItFast[x_, levelspec_:{0}, func_:Plus] := With[
  {
	  rule = (expr_func) :> factorize[expr, x]
	}
	,
  Replace[#, rule, levelspec]&
];


pullIt[xs_List, func_:Plus] := With[
  {
    rules = (x \[Function] With[{p = x},
				(func[p * b_., a_] :> p Map[(# / p) &, func[p b, a]])
			]) /@ xs
  },
  ReplaceAll[#, rules]&
];

pullIt[pattern_, func_:Plus][expr_] := With[
  {
    xs = Union@Cases[{expr}, pattern, Infinity]
  },
  pullIt[xs, func][expr]
];


powersPattern[xs_List] := Subsets[xs] // Reverse //
  Map[#^_. &, #, {2}] & //
  Apply[Times, #, {1}] & // 
  PowerExpand // ReplaceAll[x_ y_Optional :> y];

rewriteIt[Equal[lhs_, rhs_], func_] := Equal[lhs, func[rhs]]


changeSign[xs_List] := With[{
    rules = Map[x \[Function] (x^(p_.) expr_. :> (-x)^p (-1)^p expr), xs]
  },
  ReplaceAll[rules]
];
changeSign[pattern_] := Function[{expr},
  With[
    {xs = Union@Cases[{expr}, pattern, Infinity] // Flatten},
    changeSign[xs][expr]
  ]
];
changeSign[xs__] := changeSign[{xs}];


rewriteIt[funcL_, funcR_] := Function[
  {expr},
  Switch[expr,
    _Equal, funcL[First[expr]] == funcR[Last[expr]],
    _Rule, funcL[First[expr]] -> funcR[Last[expr]],
    _List, Map[rewriteIt[funcL, funcR], expr],
		_, funcL[expr] == funcR[expr]
  ]
];
rewriteIt[func_] := rewriteIt[Identity, func];


toRules = ReplaceAll[#, Equal -> Rule] &;
toEquals = ReplaceAll[#, Rule -> Equal] &;


powerExpand = modify[_Power, PowerExpand];
collectLogs = With[
  {
    rules = {
      a_. Log[x_] + a_. Log[y_] :> a Log[x y]
      , a_. Log[x_] + b_. Log[y_] :> a Log[x/y] /; a == (-b)
    }
  },
  ReplaceRepeated[#, rules] &
];
changeLogPower[power_] := ReplaceAll[#, Log[x_] :> (1/power) * Log[x^power]] &;


complexToAbs[pattern_] = ReplaceAll[#,
  {
    product:(expr_ * Conjugate[expr_]) :> Abs[expr]^2 /; MatchQ[expr, pattern]
  }
]&;


End[];


EndPackage[];
