BeginPackage["RG`Parametrization`", {"RG`Tools`",	"RG`BaseUtils`","RG`CommonNotation`",	"RG`Calculation`","LiteRed`"}]


flattenIntegrate::usage="flattenIntegrate[expr] \[LongDash] flatten out nested integrate";

nestIntegrate::usage="nestIntegrate[expr] \[LongDash] nest integrate w.r.t several variables";

indetermineIntegrate::usage="indetermineIntegrate[expr] \[LongDash] remove all integration limits";

determineIntegrate::usage="determineIntegrate[{x, low, up}][expr] \[LongDash] determine limits of integration w.r.t. x";

pushIntegrateFactors::usage="pushIntegrateFactors[x][expr] push common factors inside the integrals w.r.t. x
	pushIntegrateFactors[expr] push common factors inside the integrals w.r.t. any variables
";

integrateDelta::usage="integrateDelta[expr] \[LongDash] integrate simple DiracDelta functions
integrateDelta[expr, z] \[LongDash] integrate simple DiracDelta functions containing z as a variable";

substitute::usage="substitute[{eqs}, {oldvars}, {newvars}] \[LongDash] return lists for forward and backward substitution rules";

getParametrizationFU::usage = "getParametrizationFU[LiteRed`j, dim] \[LongDash] make an integral using U and F polynomials.
	[note]:
		(1) it contains \[Delta]-function
		(2) \[Delta]-function have the form \[Delta](1 - \[CapitalSigma]x), where sum can be taken over subset of parameters {x_i},
		(3) integration w.r.t. x_i goes from 0 to \[Infinity]
";

getParametrizationG::usage = "getParametrizationG[LiteRed`j, dim] \[LongDash] make an integral w.r.t. parameters using G polynomial.
	[note]:
		(1) integration w.r.t. x_i goes from 0 to \[Infinity]
";

getPsXsPowers::usage="getPsXsPowers[expr_integrate] \[LongDash] get polynomials, xs, and powers of polynomials from the integral parameterization. [note]: (1) `expr` must be in the form of indetermine integral w.r.t. parameters x_i, (2) integration w.r.t. x_i (in the determine form of the integral) must goes from 0 to \[Infinity]";

getRegionContribution::usage="getRegionContribution[powers, va_][region] \[LongDash] evaluate resulting `var` power, which `region` yields";


Begin["Private`"];


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


substitute[eqs:{_Equal..}, xs_List, ys_List] := Module[{
		ruleTo = Solve[eqs, xs],
		ruleFrom = Solve[eqs, ys],
		n = Length[xs]
	},
	Assert[Length[xs] === Length[ys] === Length[eqs]];

	If[(Length /@ ruleTo != {n} || Length /@ ruleFrom != {n}),
		error["non-unique substitutions, returning all possible solutions"];
		Return[{ruleTo, ruleFrom}];
	];

	Return[First /@ {ruleTo, ruleFrom}]
];


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


SetAttributes[getParametrizationFU, Listable];
getParametrizationFU[expr:(LiteRed`j[tag_, idxs__]), dim_] := Module[
	{
			ns = Select[{idxs}, (# =!= 0)&], (* non-zero powers of denominators *)
			n, U, F, xs, cf,
			result
	},
	Assert[And @@ Thread[ns > 0]];
	n = Total[ns];
	{U, F, xs} = {(-1), (-1), 1} * LiteRed`FeynParUF[LiteRed`Ds[expr], LiteRed`LMs[tag]];
	L = Length[LiteRed`LMs[tag]];
	cf = Exp[EulerGamma * L * (4 - dim)/2] * Gamma[n - L * dim / 2] / (Times@@Gamma[ns]);
	result = cf * (
		Fold[
			With[{integrand = #1, xi = #2[[1]],	ni = #2[[2]]},
				integrate[integrand * xi^(ni - 1), xi]
			]&,
			F^(L * dim/2 - n) / U^((L + 1) * dim/2 - n) * DiracDelta[1 - Total[xs]],
			Transpose[{xs, ns}]
		]
	);
	Return[result];
];


SetAttributes[getParametrizationG, Listable];
getParametrizationG[expr:(LiteRed`j[tag_, idxs__]), dim_] := Module[{
		ns = Select[{idxs}, (# =!= 0)&],
		n, U, F, G, xs, cf,
		result
	},
	Assert[And @@ Thread[ns > 0]];
	n = Total[ns];
	{U, F, xs} = {(-1), (-1), 1} * LiteRed`FeynParUF[LiteRed`Ds[expr], LiteRed`LMs[tag]];
	L = Length[LiteRed`LMs[tag]];
	cf = Exp[EulerGamma * L * (4 - dim)/2] * Gamma[dim / 2] / Gamma[(L + 1) * dim/2 - n] / (Times@@Gamma[ns]);
	G = F + U;
	result = cf * (
		Fold[
			With[{integrand = #1, xi = #2[[1]], ni = #2[[2]]},
				integrate[integrand * xi^(ni - 1), xi]
			]&,
			G^(-dim/2),
			Transpose[{xs, ns}]
		]
	);
	Return[result];
];


getPsXsPowers[integrate[integrand_, vars__]] := Module[{
		xs = {vars},
		mults, powers, ps
	},
	If[Not@MatchQ[integrand, _Symbol| _Power | Times[(_Symbol|_Power), (_Symbol|_Power)..]],
		error["integrand does not match Times[(_Symbol|_Power), (_Symbol|_Power)..]"]
		Return[{{}, {}, {}}];
	];
	mults = integrand // Replace[{expr_Times :> List@@expr, expr_ :> {expr}}];
	ps = mults // Map[Replace[{expr_Symbol :> expr, expr_Power :> expr[[1]]}]];
	powers = mults // Map[Replace[{expr_Symbol :> 1, expr_Power :> expr[[2]]}]];
	If[Times@@(ps^powers) != integrand,
		error["integrand separation fails"]
		Return[{{}, {}, {}}];
	];
	Return[{ps, xs, powers}];
];


getRegionContribution[powers_,var_][region_] := With[{
		polyScales=region[[1]],
		varScales=region[[2]]
	},
	(
		Times@@(var^(polyScales*powers)) *
		Times@@(var^varScales)
	 ) // PowerExpand // ExpandAll
];


pushIntegrateFactors[s_Symbol] := Function[expr,
	expr // ReplaceAll[a_ * integrate[b_, vars : ({s, from_, to_} | s)] :> integrate[a b, vars]]
];
pushIntegrateFactors[expr_] := ReplaceRepeated[expr, a_ *integrate[b_, vars__] :> integrate[a b, vars]]


End[];


EndPackage[];
