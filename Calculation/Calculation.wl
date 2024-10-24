(* ::Package:: *)

(* ::Text:: *)
(*Functions to perform routine transformations*)


BeginPackage["RG`Calculation`", {"RG`BaseUtils`", "RG`CommonNotation`"}];



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

force::usage = "
	force[at | limit | sum | integrate | d] forces evaluation
";


Begin["`Private`"];


Options[changeIntegrateVars] = {hold->False};
(* changeIntegrateVars[rulea:(va_ -> fb_), ruleb:(vb_ -> fa_)] := ReplaceAll[ *)
(* 	{ *)
(* 		integrate[expr_, va] :> integrate[(expr /. rulea) * D[fb, vb], vb], *)
(* 		integrate[expr_, {va, vaMin_, vaMax_}] :> integrate[(expr /. rulea) * D[fb, vb], {vb, fa /. (va -> vaMin), fa /. (va -> vaMax)}] *)
(* 	} *)
(* ]; *)
(* changeIntegrateVars[rulea:(va_ -> fb_), ruleb:(vb_ -> fa_), opts:OptionsPattern[]] := With[{ *)
(* 		det = D[fb, vb] *)
(* 	}, *)
(* 	ReplaceAll[ *)
(* 		{ *)
(* 			integrate[expr_, va] :> integrate[(expr /. rulea) * If[OptionValue[hold], Hold[Evaluate@Factor[det]], det], vb], *)
(* 			integrate[expr_, {va, vaMin_, vaMax_}] :> integrate[(expr /. rulea) * D[fb, vb], {vb, fa /. {va -> vaMin}, fa /. {va -> vaMax}}] *)
(* 		} *)
(* 	] *)
(* ]; *)

changeIntegrateVars[rulex_List, ruley_List, opts:OptionsPattern[]] := With[{
		xs = First /@ rulex,
		ys = First /@ ruley,
		fs = Last /@ rulex
	},
	With[{det = Factor[Det[Outer[D, fs, ys]]]},
		ReplaceAll[
			integrate[expr_, Sequence@@xs] :> integrate[(expr //. rulex) * If[OptionValue[hold], Hold[det], det], Sequence@@ys]
		]
	]
];

changeIntegrateVars[rules_List] := changeIntegrateVars[rules[[1]], rules[[2]]]


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

pullIntegrateFactors[] = Function[expr, With[{
  vars = expr //
    Cases[{#}, _integrate, Infinity]& //
    Map[Rest[List@@#]&] //
    ReplaceAll[{var_, a_, b_} :> var] //
    Flatten
  },
  Fold[pullIntegrateFactors[#2][#1]&, expr, vars]
]];


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

force[integrate, x_] = ReplaceAll[#,
	{
		integrate[expr_, {x, a_, b_}] :> Integrate[expr, {x, a, b}]
		, integrate[expr_, x] :> Integrate[expr, x]
	}
]&;


End[];


EndPackage[];


Print[ToString@StringForm["[info]: '``' loaded", $InputFileName]];
