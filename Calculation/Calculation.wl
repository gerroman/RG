(* ::Package:: *)

(* ::Text:: *)
(*Functions to perform routine transformations*)


BeginPackage["RG`Calculation`", {"RG`BaseUtils`", "RG`CommonNotation`"}];


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


changeIntegrateVars::usage = "
  changeIntegrateVars[va -> f[vb], vb -> g[va]] change integration variable va->vb in the integral w.r.t. va
";
changeSumVars::usage = "
  changeSumVars[va -> f[vb], vb -> g[va]] change integration variable va->vb in the integral w.r.t. va
";
setIntegrateLimits::usage = "
  setIntegrateLimits[{x, xMin, xMax}] replace indefinite integral w.r.t. x to definite
  setIntegrateLimits[{x, xMin, xMax}..]
";

pullIntegrateFactors::usage = "
  pullIntegrateFactors[va] pull out constant factor off the integrals w.r.t. va
";
pullSumFactors::usage = "
  pullSumFactors[va] pull out constant factor off the integrals w.r.t. va
";

groupIntegrals::usage = "
  groupIntegrals[va] group sum of integrals w.r.t. variable va
";
groupSums::usage = "
  groupIntegrals[va] group sum of integrals w.r.t. variable va
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


force::usage = "
  force[at | limit | sum | integrate | d] forces evaluation
";


jacobian::usage = "
  jacobian[xs, ys, rules] return jacobian matrix, J,in terms of new variables, ys, for variable transitions xs -> ys, with transitions rules of type x[i] -> f[i][ys],
	J[[i, j]] = D[y[i], x[j]] in terms of ys
";
changeVars::usage = "
  changeVars[xs, ys, rules][expr] changes variables in expr, including in differential operators
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
  With[
	  {
		  xs = Union@Cases[{expr}, pattern, Infinity]
		},
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

factorItFast[l_List, levelspec_:{0}, func_:Plus] :=
  RightComposition @@ Map[factorItFast[#, levelspec, func]&, l];

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


changeIntegrateVars[rulea:(va_ -> fb_), ruleb:(vb_ -> fa_)] := ReplaceAll[
  {
	integrate[expr_, va] :> integrate[(expr /. rulea) * D[fb, vb], vb],
	integrate[expr_, {va, vaMin_, vaMax_}] :> integrate[(expr /. rulea) * D[fb, vb], {vb, fa /. (va -> vaMin), fa /. (va -> vaMax)}]
  }
];


changeIntegrateVars[rulex_List, ruley_List] := With[{
    xs = First/@rulex,
		ys = First/@ruley,
		fs = Last/@rulex
	},
	ReplaceAll[
	  integrate[expr_, Sequence@@xs] :>
		  integrate[(expr /. rulex) * Det[Outer[D, fs, ys]], Sequence@@ys]
  ]
];

setIntegrateLimits[vx:{x_, xMin_, xMax_}] := ReplaceAll[
  integrate[expr_, y___, x, z___] :> integrate[expr, y, vx, z]
];
setIntegrateLimits[l:{_, _, _}..] := Composition @@ setIntegrateLimits /@ {l};


changeSumVars[rulea:(va_ -> fb_), ruleb:(vb_ -> fa_)] := ReplaceAll[
  {
	sum[expr_, va] :> sum[(expr /. rulea), vb],
	sum[expr_, {va, vaMin_, vaMax_}] :>	sum[(expr /. rulea), {vb, fa /. (va -> vaMin), fa /. (va -> vaMax)}]
  }
];


pullIntegrateFactors[va_] := ReplaceAll[
  {
	 integrate[expr_. * factor_, vs:{va, __}] :> factor * integrate[expr, vs] /; FreeQ[factor, va],
	 integrate[expr_. * factor_, va] :> factor * integrate[expr, va] /; FreeQ[factor, va]
  }
];

pullSumFactors[va_] := ReplaceAll[
  {
	 sum[expr_. * factor_, vs:{va, __}] :> factor * sum[expr, vs] /; FreeQ[factor, va],
	 sum[expr_. * factor_, va] :> factor * sum[expr, va] /; FreeQ[factor, va]
  }
];


groupIntegrals[va_] := ReplaceRepeated[#,
  {
  	a_. integrate[exprA_, vs:(va|{va, __})] + b_. integrate[exprB_, vs:(va|{va, __})] :>	integrate[a exprA + b exprB, vs]
  }
]&;

groupSums[va_] := ReplaceRepeated[#,
  {
	  a_. sum[exprA_, vs:(va|{va, __})]	+ b_. sum[exprB_, vs:(va|{va, __})] :> sum[a exprA + b exprB, vs]
  }
]&;


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


force[at] = ReplaceAll[#, {
  at[expr_, {x_, value_}] :> (expr /. x -> value),
  at[expr_, {x_, down_, up_}] :> (expr /. x -> up) - (expr /. x -> down)
}] &;


force[limit] = ReplaceAll[#, limit->Limit]
force[d] = ReplaceAll[#, d -> D] &;
force[dt] = ReplaceAll[#, dt -> Dt] &;
force[pd] = ReplaceAll[#, pd[expr_, var_] -> D[expr, var]] &;
force[sum] = ReplaceAll[#, sum -> Sum] &;
force[integrate] = ReplaceAll[#, integrate -> Integrate] &;
force[integrate, N] = ReplaceAll[#, integrate -> NIntegrate] &;


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


End[];


Echo[$Context];


EndPackage[];
