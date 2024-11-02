(* ::Package:: *)

(* ::Text:: *)
(*Functions to perform routine transformations*)


BeginPackage["RG`Calculation`", {"RG`BaseUtils`", "RG`Integrate`", "RG`CommonNotation`"}];


changeSumVars::usage = "
	changeSumVars[va -> f[vb], vb -> g[va]] change integration variable va->vb in the integral w.r.t. va
";

pullSumFactors::usage = "
	pullSumFactors[va] pull out constant factor off the integrals w.r.t. va
";

groupSums::usage = "
	groupIntegrals[va] group sum of integrals w.r.t. variable va
";


Begin["`Private`"];


changeSumVars[rulea:(va_ -> fb_), ruleb:(vb_ -> fa_)] := ReplaceAll[
	{
	sum[expr_, va] :> sum[(expr /. rulea), vb],
	sum[expr_, {va, vaMin_, vaMax_}] :>	sum[(expr /. rulea), {vb, fa /. (va -> vaMin), fa /. (va -> vaMax)}]
	}
];


pullSumFactors[va_] := ReplaceAll[
	{
	 sum[expr_. * factor_, vs:{va, __}] :> factor * sum[expr, vs] /; FreeQ[factor, va],
	 sum[expr_. * factor_, va] :> factor * sum[expr, va] /; FreeQ[factor, va]
	}
];

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


End[];


EndPackage[];


Print[ToString@StringForm["[info]: '``' loaded", FileNameTake[$InputFileName, -3]]];
