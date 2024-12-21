Get["RG/Parametrization/Rules.wl"]


BeginPackage["RG`Parametrization`",
  {"LiteRed`","GetRegions`", "RG`Tools`", "RG`Notation`Integrate`"}
]


getParametrizationFU::usage = "getParametrizationFU[LiteRed`j, dim] \[LongDash] make an integral using U and F polynomials.\n[note]: (1) it contains \[Delta]-function in the form \[Delta](1 - \[CapitalSigma]x)\n[note]: (2) integration w.r.t. x_i goes from 0 to \[Infinity]\n[note]: (3) it is assumed that the denominatos have Euclidian form: [-(l + p)^2 + m^2 - i0]";


getParametrizationG::usage = "getParametrizationG[LiteRed`j, dim] \[LongDash] make an integral w.r.t. parameters using G polynomial.\n[note]: (1) integration w.r.t. x_i goes from 0 to \[Infinity]";


getPsXsPowers::usage="getPsXsPowers[expr_integrate] \[LongDash] get polynomials, xs, and powers of polynomials from the integral parameterization.\n[note]: (1) `expr` must be in the form of indetermine integral w.r.t. parameters x_i,\n[note]: (2) integration w.r.t. x_i must goes from 0 to \[Infinity]";


getRegionContribution::usage="getRegionContribution[powers, va_][region] \[LongDash] evaluate resulting `var` power, which `region` yields";


getRegions::usage="getRegions[integral, delta] \[LongDash] get regions "


pullOut::usage="pullOut[x] \[LongDash] pull x out of powers in Feynman parameter interals assuming x is positive\n[note]: it is use hold[]/release internally"


Begin["Private`"]


(*(Lee, 2013), Eq.(2.2)*)
(*Additional factor:*)
(*Exp[EulerGamma * L * (4 - dim)/2]*)

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
  cf = Exp[EulerGamma * L * (4 - dim)/2] *
    Gamma[n - L * dim / 2] / (Times@@Gamma[ns]);
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


(*(Lee, 2013), Eq.(2.5)*)
(*Additional factor:*)
(*Exp[EulerGamma * L * (4 - dim)/2]*)

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
  cf = Exp[EulerGamma * L * (4 - dim) / 2] *
    Gamma[dim / 2] /
    (Gamma[(L + 1) * dim/2 - n] * (Times@@Gamma[ns]));
  G = F + U + Global`mi0;
  result = cf * (
    Fold[
      With[{integrand = #1, xi = #2[[1]], ni = #2[[2]]},
        integrate[integrand * xi^(ni - 1), {xi, 0, Infinity}]
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
        Composition @@ (determineIntegrate[{#,0,Infinity}]&/@xs) //
        flattenIntegrate
    }
  ]]];
  Array[func, Length[regions]]
];


pullOut[xs_List] := RightComposition @@ (pullOut /@ xs)
pullOut[x_] = Function[{expr},
  expr //
    pullIt[x] //
    ReplaceRepeated[#, rule`mi0[{x,1/x}]]& //
    powerExpand[{x,1/x}]
]
pullOut[xs__] := pullOut[{xs}]


End[]


EndPackage[]


Print[ToString@StringForm["[info]: '``' loaded", FileNameTake[$InputFileName, -3]]];
