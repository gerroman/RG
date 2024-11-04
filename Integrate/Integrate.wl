(* ::Package:: *)


BeginPackage["RG`Integrate`", {"RG`Tools`"}]


integrate::usage = "integrate[expr, region] represent integral";


changeIntegrateVars::usage = "changeIntegrateVars[va -> f[vb], vb -> g[va]] \[LongDash]  change integration variable va->vb in the integral w.r.t. va";

pullIntegrateFactors::usage = "pullIntegrateFactors[va] \[LongDash] pull out constant factor off the integrals w.r.t. va";

groupIntegrals::usage = "groupIntegrals[va]  \[LongDash] group sum of integrals w.r.t. variable va";

substitute::usage="substitute[{eqs}, {oldvars}, {newvars}] \[LongDash] return lists for forward and backward substitution rules";

flattenIntegrate::usage="flattenIntegrate[expr] \[LongDash] flatten out nested integrate";

nestIntegrate::usage="nestIntegrate[expr] \[LongDash] nest integrate w.r.t several variables";

indetermineIntegrate::usage="indetermineIntegrate[expr] \[LongDash] remove all integration limits";

determineIntegrate::usage="determineIntegrate[{x, low, up}][expr] \[LongDash] determine limits of integration w.r.t. x";


Begin["`Private`"]


integrate /: Format[integrate[expr_, region__], TraditionalForm] := (
  HoldForm[Integrate[expr, region]]
);


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
	With[{det = Factor[Det[Outer[D, Last /@ First[ruleTo], ys]]]},
	  Print["[jacobian]: ", Hold[det]]
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
			integrate[expr_, Sequence@@xs] :> integrate[(expr //. rulex) * If[OptionValue[Abs], Abs[det], det], Sequence@@ys]
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
	}
];

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


force[integrate] = ReplaceAll[#, integrate -> Integrate] &;

force[integrate, N] = ReplaceAll[#, integrate -> NIntegrate] &;

force[integrate, x_] = ReplaceAll[#, {
 integrate[expr_, {x, a_, b_}] :> Integrate[expr, {x, a, b}], 
 integrate[expr_, x] :> Integrate[expr, x]
}]&;


flattenIntegrate[expr_] := ReplaceRepeated[
	expr,
	integrate[(d_.) integrate[a_, b_], c__] :> integrate[d * a, c, b]
];


nestIntegrate[expr_] := ReplaceRepeated[
	expr,
	integrate[a_, b__, c_] :> integrate[integrate[a, c], b]
];


indetermineIntegrate[expr_] := ReplaceAll[
	(expr // flattenIntegrate),
	integrate[a_, b__] :> integrate[a, Sequence @@ (First /@ Flatten /@ List /@ {b})]
];


determineIntegrate[{x_, low_, up_}][expr_] := ReplaceAll[
	expr,
	{
		integrate[a_, x] :> integrate[a, {x, low, up}],
		integrate[a_, b___, x, c___] :> integrate[integrate[a, {x, low, up}], b, c]
	}
];


groupIntegrals[va_] := ReplaceRepeated[#, {
  a_. integrate[exprA_, vs:(va|{va, __})] + b_. integrate[exprB_, vs:(va|{va, __})] :>	
	  integrate[a exprA + b exprB, vs]
}]&;


End[]


EndPackage[]


Print[ToString@StringForm["[info]: '``' loaded", FileNameTake[$InputFileName, -3]]];
