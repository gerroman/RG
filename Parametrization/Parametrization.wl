Global`mi0::usage="mi0 \[LongDash] imaginary part of the propagators denominators (- \[ImaginaryI] \[CurlyEpsilon])"

rule`mi0::usage = "rule`mi0  \[LongDash] replaces mi0 * (_Hold) -> mi0"

rule`imaginary::usage "rule`imaginary  \[LongDash] substitute (expr - \[ImaginaryI] \[CurlyEpsilon])^p assuming expr is negative"


rule`f21::usage="rule`f21 \[LongDash] substitute integral representation of Hypergeometric2F1"
rule`f21arg::usage = "rule`f21arg \[LongDash] argument shift {z -> 1 - z} of Hypergeometric2F1"
rule`f21ind::usage = "rule`f21ind \[LongDash] index shift of {} Hypergeometric2F1"

rule`appell::usage="rule`f21 \[LongDash] substitute integral representation of AppellF1"


Begin["rule`Private`"]


(* ::Section:: *)
(* Правила работы с мнимыми добавками *)
Format[Global`mi0, TraditionalForm] := DisplayForm[RowBox[{"(","-","\[ImaginaryI]","0",")"}]];
Unprotect[Plus];
Format[Plus[Global`mi0, a_], TraditionalForm] := DisplayForm[RowBox[{ToBoxes[a,TraditionalForm], "-","\[ImaginaryI]0"}]];
Format[Plus[(-1)*Global`mi0, a_], TraditionalForm] := DisplayForm[RowBox[{ToBoxes[a,TraditionalForm], "+","\[ImaginaryI]0"}]];
Protect[Plus];


rule`mi0[x_] := Global`mi0 * x^(p_.) -> Global`mi0;
rule`imaginary = (expr_ + Global`mi0)^(p_.) :> (
  Print[ToString[StringForm["assuming `` < 0", expr], InputForm]];
	(-expr)^p Exp[-I Pi p]
);
rule`imaginary = (expr_ - Global`mi0)^(p_.) :> (
  Print[ToString[StringForm["assuming `` < 0", expr], InputForm]];
	(-expr)^p Exp[I Pi p]
);


(* ::Section:: *)
(* Правила работы с гипергеометрической функцией *)

rule`f21 = {
  integrate[(1 - tau_)^(q_.)*(tau_)^(p_.)*(1 + (tau_)*(z_))^r_., {tau_, 0, 1}] :> 
	  (Gamma[1 + p]*Gamma[1 + q] / Gamma[2 + p + q]) * 
			 Hypergeometric2F1[-r, 1 + p , 2 + p + q, -z],
	integrate[(1 - tau_)^(q_.)*(1 + (tau_) * (z_))^(r_.), {tau_, 0, 1}] :> 
	  (q + 1)^(-1) * Hypergeometric2F1[-r, 1, 2 + q, -z], 
	integrate[(tau_)^(p_.)*(1 + (tau_)*(z_))^(r_.), {tau_, 0, 1}] :> 
    (p + 1)^(-1) * Hypergeometric2F1[-r, 1 + p, 2 + p, -z],
	integrate[(tau_)^(p_.) (1 + (tau_)*(z_))^(r_.) (1 + tau_)^(s_.), {tau_, 0, Infinity}] :> With[
    {q = -(s + 2 + p + r)},
	  (Gamma[1 + p]*Gamma[1 + q] / Gamma[2 + p + q]) * 
			 Hypergeometric2F1[-r, 1 + p , 2 + p + q, 1-z]
	],
	integrate[(1 +(tau_) (z_))^(r_.) (1 + (tau_))^(s_.), {tau_, 0, Infinity}] :> With[
	  {q = -(s + 2 + r)},
	  (q + 1)^(-1) * Hypergeometric2F1[-r, 1, 2 + q, 1-z]
	],
	integrate[(tau_)^(p_.) (1 + (tau_) (z_))^(r_.), {tau_, 0, Infinity}] :> With[
    {q = -(2 + p + r)},
	  (Gamma[1 + p]*Gamma[1 + q] / Gamma[2 + p + q]) * 
		Hypergeometric2F1[-r, 1 + p , 2 + p + q, 1 - z]
	]
}


rule`f21ind = {
  Hypergeometric2F1[a_, b_, c_, z_] :> 
	  Together[(1 - z)]^(-a) Hypergeometric2F1[a, c - b, c, Together[z/(z - 1)]]
}

rule`f21arg = (
  (f_.) Hypergeometric2F1[a_, b_, c_, z_] :> 
	  f ((Gamma[c]*Gamma[c - a - b])/(Gamma[c - a]*Gamma[c - b])) * 
      Hypergeometric2F1[a, b, a + b + 1 - c, 1 - z] + 
		f ((Gamma[c]*Gamma[a + b - c])/(Gamma[a]*Gamma[b])) * 
		  (1 - z)^(c - a - b) * Hypergeometric2F1[c - a, c - b, 1 + c - a - b, 1 - z]
)

rule`appell = {
	integrate[t_^p_. (1 - t_)^q_. (1 + t_ x_)^r_. (1 + t_ y_)^s_., {t_, 0, 1}] :> 
	  (Gamma[p + 1] Gamma[q + 1])/ Gamma[p + q + 2] *
  	  AppellF1[p + 1, -r, -s, p + q + 2, -x, -y], 
	integrate[(1 - t_)^q_. (1 + t_ x_)^r_. (1 + t_ y_)^s_., {t_, 0, 1}] :> 
	  (Gamma[1] Gamma[q + 1])/ Gamma[q + 2] * 
		  AppellF1[1, -r, -s, q + 2, -x, -y], 
	integrate[t_^p_. (1 + t_ x_)^r_. (1 + t_ y_)^s_., {t_, 0, 1}] :> 
	  (Gamma[p + 1] Gamma[1]) / Gamma[p + 2] * 
		  AppellF1[p + 1, -r, -s, p + 2, -x, -y], 
	integrate[(1 + t_ x_)^r_. (1 + t_ y_)^s_., {t_, 0, 1}] :> 
	  (Gamma[1] Gamma[1])/Gamma[2] *
		  AppellF1[1, -r, -s, 2, -x, -y]
}


End[]


BeginPackage["RG`Parametrization`", {"LiteRed`","GetRegions`", "RG`Tools`", "RG`Integrate`"}]


integrateDelta::usage="integrateDelta[expr] \[LongDash] integrate simple DiracDelta functions
integrateDelta[expr, z] \[LongDash] integrate simple DiracDelta functions containing z as a variable";


getParametrizationFU::usage = "getParametrizationFU[LiteRed`j, dim] \[LongDash] make an integral using U and F polynomials.\n[note]: (1) it contains \[Delta]-function in the form \[Delta](1 - \[CapitalSigma]x)\n[note]: (2) integration w.r.t. x_i goes from 0 to \[Infinity]\n[note]: (3) it is assumed that the denominatos have Euclidian form: [-(l + p)^2 + m^2 - i0]";


getParametrizationG::usage = "getParametrizationG[LiteRed`j, dim] \[LongDash] make an integral w.r.t. parameters using G polynomial.\n[note]: (1) integration w.r.t. x_i goes from 0 to \[Infinity]";


getPsXsPowers::usage="getPsXsPowers[expr_integrate] \[LongDash] get polynomials, xs, and powers of polynomials from the integral parameterization.\n[note]: (1) `expr` must be in the form of indetermine integral w.r.t. parameters x_i,\n[note]: (2) integration w.r.t. x_i must goes from 0 to \[Infinity]";


getRegionContribution::usage="getRegionContribution[powers, va_][region] \[LongDash] evaluate resulting `var` power, which `region` yields";


getRegions::usage="getRegions[integral, delta] \[LongDash] get regions "


pullOut::usage="pullOut[x] \[LongDash] pull x out of powers in Feynman parameter interals assuming x is positive\n[note]: it is use hold[]/release internally"



Begin["Private`"];


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
			(F + Global`mi0)^(L * dim/2 - n) / U^((L + 1) * dim/2 - n) * DiracDelta[1 - Total[xs]],
			Transpose[{xs, ns}]
		]
	) // flattenIntegrate;
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
	) // flattenIntegrate;
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


Options[getRegions]={"verbose"->True};
getRegions[integral_, delta_, opts:OptionsPattern[]] := Module[
  {int, ps, xs, powers, regions, contrib, vars, subs, func, pos, v, mis, verbose=OptionValue["verbose"]},
	int = integral //
	  indetermineIntegrate //
	  modify[_Power, Map[Expand]]//
		modify[_Plus, Expand];
	If[verbose, Print[int]];
  {ps, xs, powers} = getPsXsPowers[int];
  If[verbose, Print[{ps, xs, powers}]];
	mis = (ps // Replace[#, {(f_.) Global`mi0 + _. :> f Global`mi0, _:>0}, {1}]&);
	If[verbose, Print[mis]];
  regions = GetRegions[ps/.{Global`mi0->0}, xs, delta];
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
				  Factor[ps/.{Global`mi0->0}/.subs[[1]]] / delta^regions[[pos, 1]] //
					ReplaceAll[delta->0] // Expand)
					+ mis
				)^powers
			]
    },
    {
      contrib[[pos]],
      integrate[(First[int] /. psSubs), Sequence@@xs] //
        changeIntegrateVars[Sequence@@subs, Abs->False] //
        ReplaceAll[Thread[vars->xs]] //
        ReplaceAll[contrib[[pos]] -> 1] //
				RightComposition @@ (determineIntegrate[{#,0,Infinity}]&/@xs) //
				flattenIntegrate
    }
  ]]];
  Array[func, Length[regions]]
];


pullOut[xs_List] := RightComposition@@(pullOut /@ xs)
pullOut[x_] = Function[{expr},
  expr //
	  hold[x] //
	  pullIt[Hold[x]] //
		ReplaceAll[rule`mi0[Hold[x]]] //
		powerExpand[Hold[x]] //
		release[x]
]
pullOut[xs__] := pullOut[{xs}]


End[];


EndPackage[];


Print[ToString@StringForm["[info]: '``' loaded", FileNameTake[$InputFileName, -3]]];
