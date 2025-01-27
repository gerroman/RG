(* ::Package:: *)


BeginPackage["RG`Notation`Integrate`", {"RG`Notation`Force`", "RG`Notation`D`"}]


integrate::usage = "integrate[expr, region] represent an integral"
changeIntegrateVars::usage = "changeIntegrateVars[va -> f[vb], vb -> g[va]] \[LongDash]  change integration variable va->vb in the integral w.r.t. va"
pullIntegrateFactors::usage = "pullIntegrateFactors[va] \[LongDash] pull out constant factor off the integrals w.r.t. va"
groupIntegrals::usage = "groupIntegrals[va]  \[LongDash] group sum of integrals w.r.t. variable va"
substitute::usage = "substitute[{eqs}, {oldvars}, {newvars}] \[LongDash] return lists for forward and backward substitution rules"
flattenIntegrate::usage = "flattenIntegrate[expr] \[LongDash] flatten out nested integrate"
nestIntegrate::usage = "nestIntegrate[expr] \[LongDash] nest integrate w.r.t several variables"
indetermineIntegrate::usage = "indetermineIntegrate[expr] \[LongDash] remove all integration limits"
determineIntegrate::usage = "determineIntegrate[{x, low, up}][expr] \[LongDash] determine limits of integration w.r.t. x"
reorderIntegrate::usage = "reorderIntegrate[x][expr] \[LongDash] set the outer integration w.r.t. x"


integrateDelta::usage="integrateDelta[expr] \[LongDash] integrate simple DiracDelta functions
integrateDelta[expr, z] \[LongDash] integrate simple DiracDelta functions containing z as a variable";


Begin["`Private`"]


integrate /: Format[integrate[expr_, ls__], TraditionalForm] := DisplayForm[
  RowBox[{
    RowBox[
      Which[
        # === Global`\[Ellipsis], Global`\[Ellipsis],
        Head[#]===List && Length[#] == 2, RowBox[{"\[Integral]", SuperscriptBox["\[DifferentialD]", TraditionalForm[#[[2]]]],  TraditionalForm[#[[1]]]}],
        Head[#]===List && Length[#] == 3, RowBox[{SubsuperscriptBox["\[Integral]",TraditionalForm[#[[2]]], TraditionalForm[#[[3]]]], "\[DifferentialD]", TraditionalForm[#[[1]]]}],
        True, RowBox[{"\[Integral]", "\[DifferentialD]", TraditionalForm[#]}]
      ]& /@
      {ls}
    ],
    "(", ToBoxes[expr,TraditionalForm], ")"
  }]
];

integrate /: Format[integrate[expr_, {k_, d_}, "factor" -> factor_], TraditionalForm] := With[{
		num = If[Numerator[factor] =!= 1, ToBoxes[Numerator[factor], TraditionalForm], Nothing]
		,
		den = Denominator[factor]
	}
	,
	DisplayForm[RowBox[{
		"\[Integral]",
		If[den =!= 1,
			FractionBox[
				RowBox[{num, SuperscriptBox["\[DifferentialD]", TraditionalForm[d]], ToBoxes[k, TraditionalForm]}]
				,
				ToBoxes[den, TraditionalForm]
			]
			,
			RowBox[{num, SuperscriptBox["\[DifferentialD]", TraditionalForm[d]], TraditionalForm[k]}]
		]
		,
		ToBoxes[expr, TraditionalForm]
	}]]
]


integrate /: Format[integrate[expr_, var_, "factor" -> factor_], TraditionalForm] := With[{
		num = If[Numerator[factor] =!= 1, ToBoxes[Numerator[factor], TraditionalForm], Nothing]
		,
		den = Denominator[factor]
		,
		k = If[Head[var] === List,
			var[[1]]
			,
			var
		]
		,
		int = If[Head[var] === List,
			SubsuperscriptBox["\[Integral]", ToBoxes[var[[2]], TraditionalForm], ToBoxes[var[[3]], TraditionalForm]]
			,
			"\[Integral]"
		]
	}
	,
	DisplayForm[RowBox[{
		int
		,
		If[den =!= 1,
			FractionBox[
				RowBox[{num, "\[DifferentialD]", ToBoxes[k, TraditionalForm]}]
				,
				ToBoxes[den, TraditionalForm]
			]
			,
			RowBox[{num, "\[DifferentialD]", ToBoxes[k, TraditionalForm]}]
		]
		,
		ToBoxes[expr, TraditionalForm]
	}]]
]

Format[integrate[expr_],TraditionalForm] := DisplayForm@RowBox[{"\[Integral]", ToBoxes[expr,TraditionalForm]}]


substitute[eqs:{_Equal..}, xs_List, ys_List] := Module[{
    ruleTo = Solve[eqs, xs],
    ruleFrom = Solve[eqs, ys],
    n = Length[xs]
  },
  Assert[Length[xs] === Length[ys] === Length[eqs]];
  If[(Length /@ ruleTo != {n} || Length /@ ruleFrom != {n}),
    Print["[warning]: non-unique substitutions, returning all possible solutions"];
    Return[{ruleTo, ruleFrom}];
  ];
  Return[First /@ {ruleTo, ruleFrom}]
];
substitute[eqs:(_Equal).., xs_List, ys_List] := substitute[{eqs}, xs, ys];
substitute[eqs_Equal, x_, y_] := substitute[{eqs}, {x}, {y}];


Options[changeIntegrateVars] = {Abs->True};
changeIntegrateVars[rulex:{_Rule..}, ruley:{_Rule..}, opts:OptionsPattern[]] := With[{
    xs = First /@ rulex,
    ys = First /@ ruley,
    fs = Last /@ rulex
  },
  With[{det = Factor[Det[Outer[D, fs, ys]]]},
    ReplaceAll[
      integrate[expr_, Sequence@@xs] :>
        integrate[
          (expr //. rulex) *
            If[OptionValue[Abs], Abs[det], det],
          Sequence@@ys
        ]
    ]
  ]
];
changeIntegrateVars[rules:{{_Rule..}, {_Rule..}}, opts:OptionsPattern[]] := (
  changeIntegrateVars[rules[[1]], rules[[2]], opts]
);
changeIntegrateVars[eqs:{_Equal..}, xs_List, ys_List, opts:OptionsPattern[]] := (
  changeIntegrateVars[substitute[eqs, xs, ys], opts]
);
changeIntegrateVars[eqs:(_Equal).., xs_List, ys_List, opts:OptionsPattern[]] := (
  changeIntegrateVars[substitute[{eqs}, xs, ys], opts]
);
changeIntegrateVars[eqs_Equal, xs_Symbol, ys_Symbol, opts:OptionsPattern[]] := (
  changeIntegrateVars[substitute[{eqs}, {xs}, {ys}], opts]
);


pullIntegrateFactors[va_/; FreeQ[va, integrate]] := ReplaceAll[{
  integrate[expr_. * factor_, vs:{va, __}] :>
    factor * integrate[expr, vs] /; FreeQ[factor, va],
        integrate[expr_. * factor_, va] :>
    factor * integrate[expr, va] /; FreeQ[factor, va]
}];

pullIntegrateFactors[] = Function[expr, With[{
  vars = expr //
    Cases[{#}, _integrate, Infinity]& //
    Map[Rest[List@@#]&] //
    ReplaceAll[{var_, a_, b_} :> var] //
    Flatten
  },
  Fold[pullIntegrateFactors[#2][#1]&, expr, vars]
]];
pullIntegrateFactors[expr_] := pullIntegrateFactors[][expr]


flattenIntegrate[expr_] := ReplaceRepeated[
  expr,
  integrate[(d_.) integrate[a_, b__], c__] :> integrate[d * a, c, b]
];


nestIntegrate[expr_] := ReplaceRepeated[
  expr,
  integrate[a_, b__, c_] :> integrate[integrate[a, c], b]
];

nestIntegrate[expr_, x_] := ReplaceAll[
  expr,
  {
    integrate[a_, b___, x, c___] :> integrate[integrate[a, x], b, c] /; Length[{b,c}] > 0,
    integrate[a_, b___, range:{x, ___}, c___] :> integrate[integrate[a, range], b, c] /; Length[{b,c}] > 0
  }
]
nestIntegrate[expr_, xs_List] := Fold[nestIntegrate, expr, xs]


indetermineIntegrate[expr_] := ReplaceAll[
  (expr // flattenIntegrate),
  integrate[a_, b__] :> integrate[a, Sequence @@ (First /@ Flatten /@ List /@ {b})]
];
indetermineIntegrate[expr_, var_] := ReplaceAll[
  expr,
  integrate[a_, b___,{var, range___}, c___] :> integrate[a, b, var, c]
]


determineIntegrate[{x_, low_, up_}][expr_] := ReplaceAll[
  expr,
  {
    integrate[a_, x] :> integrate[a, {x, low, up}],
    integrate[a_, b___, x, c___] :> integrate[integrate[a, {x, low, up}], b, c]
  }
];

determineIntegrate[{x_, d_}][expr_] := ReplaceAll[
  expr,
  {
    integrate[a_, x] :> integrate[a, {x, d}],
    integrate[a_, b___, x, c___] :> integrate[integrate[a, {x, d}], b, c]
  }
];


groupIntegrals[va_] := ReplaceRepeated[#, {
  a_. integrate[exprA_, vs:(va|{va, __})] + b_. integrate[exprB_, vs:(va|{va, __})] :>
    integrate[a exprA + b exprB, vs]
}]&;




reorderIntegrate[x_] := ReplaceAll[
  integrate[a_, b___, var:(x|{x,__}), c___] :> integrate[a, var, b, c]
]

reorderIntegrate[xs__] := Composition @@ (reorderIntegrate/@{xs})


integrateDelta[iexpr_] := With[{delta = DiracDelta}, ReplaceAll[
  iexpr,
  {
    (integrate[(expr_.) delta[r_ + s_.],  z_] /; ((-r == z) && FreeQ[s, z])) :> (expr /. {z -> s}),
    (integrate[(expr_.) delta[r_ + s_.], x___, z_,  y___] /; ((-r == z) && FreeQ[s, z])) :> integrate[(expr /. {z -> s}), x, y],
    (integrate[(expr_.) delta[r_ + s_.],  z_] /; ((r == z) && FreeQ[s, z])) :> (expr /. {z -> -s}),
    (integrate[(expr_.) delta[r_ + s_.], x___, z_,  y___] /; ((r == z) && FreeQ[s, z])) :> integrate[(expr /. {z -> -s}), x, y],
    (integrate[(expr_.) delta[r_ + s_.], {z_, a_, b_}] /; ((-r == z) && FreeQ[s, z])) :> (expr /. {z -> s})  * HeavisideTheta[s - a] * HeavisideTheta[b - s],
    (integrate[(expr_.) delta[r_ + s_.], x___, {z_, a_, b_},  y___] /; ((-r == z) && FreeQ[s, z])) :>	integrate[(expr /. {z -> s}) * HeavisideTheta[s - a] * HeavisideTheta[b - s], x, y],
    (integrate[(expr_.) delta[r_ + s_.], {z_, a_, b_}] /; ((r == z) && FreeQ[s, z])) :>	(expr /. {z -> -s})  * HeavisideTheta[-s - a] * HeavisideTheta[b + s],
    (integrate[(expr_.) delta[r_ + s_.], x___, {z_, a_, b_},  y___] /; ((r == z) && FreeQ[s, z])) :> integrate[(expr /. {z -> -s}) * HeavisideTheta[-s - a] * HeavisideTheta[b + s], x, y]
  }
]];


integrateDelta[iexpr_, z_] := With[{delta = DiracDelta}, ReplaceAll[
  iexpr,
  {
    (integrate[(expr_.) delta[r_ + s_.],  z] /; ((-r == z) && FreeQ[s, z])) :> (expr /. {z -> s}),
    (integrate[(expr_.) delta[r_ + s_.], x___, z,  y___] /; ((-r == z) && FreeQ[s, z])) :> integrate[(expr /. {z -> s}), x, y],
    (integrate[(expr_.) delta[r_ + s_.],  z] /; ((r == z) && FreeQ[s, z])) :> (expr /. {z -> -s}),
    (integrate[(expr_.) delta[r_ + s_.], x___, z,  y___] /; ((r == z) && FreeQ[s, z])) :> integrate[(expr /. {z -> -s}), x, y],
    (integrate[(expr_.) delta[r_ + s_.], {z, a_, b_}] /; ((-r == z) && FreeQ[s, z])) :> (expr /. {z -> s})  * HeavisideTheta[s - a] * HeavisideTheta[b - s],
    (integrate[(expr_.) delta[r_ + s_.], x___, {z, a_, b_},  y___] /; ((-r == z) && FreeQ[s, z])) :> integrate[(expr /. {z -> s}) * HeavisideTheta[s - a] * HeavisideTheta[b - s], x, y],
    (integrate[(expr_.) delta[r_ + s_.], {z, a_, b_}] /; ((r == z) && FreeQ[s, z])) :> (expr /. {z -> -s})  * HeavisideTheta[-s - a] * HeavisideTheta[b + s],
    (integrate[(expr_.) delta[r_ + s_.], x___, {z, a_, b_},  y___] /; ((r == z) && FreeQ[s, z])) :> integrate[(expr /. {z -> -s}) * HeavisideTheta[-s - a] * HeavisideTheta[b + s], x, y]
  }
]];


force[integrate, opts:OptionsPattern[]] := (
  ReplaceAll[#, integrate -> (Integrate[##, opts]&)]&
)
force[integrate, N, opts:OptionsPattern[]] := (
  ReplaceAll[#, integrate -> (NIntegrate[##, opts]&)] &
);
force[integrate, x_, opts:OptionsPattern[]] := ReplaceAll[#, {
  integrate[expr_, {x, a_, b_}] :> Integrate[expr, {x, a, b}, opts],
  integrate[expr_, x] :> Integrate[expr, x, opts]
}]&;


End[]


EndPackage[]


RG`Scripts`fileStamp[]
