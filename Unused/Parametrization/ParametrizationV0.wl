BeginPackage["RG`Parametrization`", {"RG`Tools`",	"RG`BaseUtils`","RG`CommonNotation`",	"RG`Calculation`","LiteRed`","GetRegions`"}]

integrateDelta::usage="integrateDelta[expr] \[LongDash] integrate simple DiracDelta functions
integrateDelta[expr, z] \[LongDash] integrate simple DiracDelta functions containing z as a variable";

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


getRegions::usage="getRegions[integral, delta] \[LongDash] get region contribution"


mi0::usage="mi0"


pull::usage = "pull[expr] \[LongDash] pulls out expression from powers, assuming it is positive"


Begin["Private`"];


Format[mi0, TraditionalForm] := DisplayForm[RowBox[{"(","-","\[ImaginaryI]","0",")"}]];

Unprotect[Plus];
  Format[Plus[mi0, a_], TraditionalForm] := DisplayForm[RowBox[{ToBoxes[a,TraditionalForm], "-","\[ImaginaryI]0"}]];
  Format[Plus[(-1)*mi0, a_], TraditionalForm] := DisplayForm[RowBox[{ToBoxes[a,TraditionalForm], "+","\[ImaginaryI]0"}]];
Protect[Plus];


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
			(F + mi0)^(L * dim/2 - n) / U^((L + 1) * dim/2 - n) * DiracDelta[1 - Total[xs]],
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
	If[Not@MatchQ[integrand, _Symbol| _Power |_Plus | Times[(_Symbol|_Power|_Plus), (_Symbol|_Power|_Plus)..]],
		error["integrand has unexpected form"];
		Return[{{}, {}, {}}];
	];
	mults = integrand // Replace[{expr_Times :> List@@expr, expr_ :> {expr}}];
	ps = mults // Map[Replace[{expr_Symbol :> expr, expr_Plus :> expr, expr_Power :> expr[[1]]}]];
	powers = mults // Map[Replace[{expr_Symbol :> 1, expr_Plus :> 1, expr_Power :> expr[[2]]}]];
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


Options[getRegions]={
  "verbose"->True
}

getRegions[integral_, delta_, opts:OptionsPattern[]] := Module[
  {int, ps, xs, powers, regions, contrib, vars, subs, func, pos, v, mis, verbose=OptionValue["verbose"]},
	int = integral //
	  indetermineIntegrate //
	  modify[_Power, Map[Expand]]//
		modify[_Plus, Expand];
	If[verbose, Print[int]];
  {ps, xs, powers} = getPsXsPowers[int];
  If[verbose, Print[{ps, xs, powers}]];
	mis = (ps // Replace[#, {(f_.)mi0 + a_. :> f mi0, _:>0}, {1}]&);
	If[verbose, Print[mis]];
  regions = GetRegions[ps/.{mi0->0}, xs, delta];
	If[verbose, Print[regions]];
  contrib = getRegionContribution[powers, delta] /@ regions;
	If[verbose, Print[contrib]];
  vars = Array[v, Length[xs]];
  func = Function[{pos},
    With[{
      subs = substitute[Thread[xs == vars*delta^regions[[pos, 2]]], xs, vars]
    },
    With[{
      psSubs = Thread[
			  ps^powers -> delta^Expand[(regions[[pos, 1]] * powers)] * ((
				  Factor[ps/.{mi0->0}/.subs[[1]]] / delta^regions[[pos, 1]] //
					ReplaceAll[delta->0] // Expand)
					+ mis
				)^powers
			]
    },
    {
      contrib[[pos]],
      integrate[(First[int] /. psSubs), Sequence@@xs] //
        changeIntegrateVars[Sequence@@subs] //
        ReplaceAll[Thread[vars->xs]] //
        ReplaceAll[contrib[[pos]] -> 1] //
				RightComposition@@(determineIntegrate[{#,0,Infinity}]&/@xs) //
				flattenIntegrate
    }
  ]]];
  Array[func, Length[regions]]
];


rule`mi = Times[mi0, (_Hold^_.) ..] -> mi0;
rule`imaginary = (expr_ + mi0)^p_. :> (-expr)^p Exp[-I Pi p];
rule`pow = (
  Times[expr_, factor : (_Hold^_.) ..]^p_ :> 
    PowerExpand[ReleaseHold[PowerExpand[(Times[factor])^p]]] expr^p
);


pull[factor_] := RightComposition[
  pullIt[factor],
  hold[{1/factor, factor}],
  ReplaceAll[rule`mi],
  ReplaceAll[rule`pow],
  ReleaseHold
]
pull[factors_List] := RightComposition @@ (pull /@ factors);
pull[factors__] := pull[{factors}]



End[];


EndPackage[];


Print[ToString@StringForm["[info]: '``' loaded", FileNameTake[$InputFileName, -3]]];
