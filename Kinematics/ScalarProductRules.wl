BeginPackage["RG`Kinematics`ScalarProductRules`", {
  "RG`Kinematics`ScalarProduct`",
  "RG`Tools`"
}]


GetScalarProductRules::usage="GetScalarProductRules[{p1, \[Ellipsis], pn}, eqs:{_Equal..}] \[LongDash] get rules to substitute scalar products assuming conservation p1 + \[Ellipsis] + pn == 0 and constraints eqs"


EvaluateScalarProducts::usage="EvaluateScalarProducts[expr] \[LongDash] evaluates scalar products using distributive and associative laws\nEvaluateScalarProducts[expr, rules] \[LongDash] evalates scalar products using rules"


Begin["`Private`"]


EvaluateScalarProducts[expr_] := expr //
  RG`Tools`distribute[ScalarProduct] //
  RG`Tools`pullFactor[factor_/;(NumberQ[factor]||NumberQ[factor]), ScalarProduct]

EvaluateScalarProducts[expr_, rules_] := expr //
  RG`Tools`distribute[ScalarProduct] //
  RG`Tools`pullFactor[factor_/;(NumberQ[factor]||NumberQ[factor]), ScalarProduct] //
  ReplaceAll[rules]


GetScalarProductRules::badsps = "can not construct `` different scalar products using ``"
GetScalarProductRules::badeqs = "inconsistent equations for `` in the constraints ``"
GetScalarProductRules::lowrank = "found `` of `` scalar products rules using constraints"
GetScalarProductRules::reduce = "can not derive dependent scalar products in the matrix ``"
GetScalarProductRules[ps_List, eqs_List:{}] := Module[{
    sps, p2s, n = Length[ps], nSps,
    eqsConservation,
    eqsExpand, eqsMatrix, eqsRHS, eqsRowReduced, rank,
    ruleEqs,
    i, j, tmp
  },
  p2s = Inner[ScalarProduct, ps, ps, List] //
    EvaluateScalarProducts //
    MapAt[Replace[(expr_/;(NumberQ[expr]||NumericQ[expr])) * sp_ScalarProduct :> sp], #, {All,All}]& //
    Flatten //
    Union;
  sps = Outer[ScalarProduct, ps, ps] //
    EvaluateScalarProducts //
    MapAt[Replace[(expr_/;(NumberQ[expr]||NumericQ[expr])) * sp_ScalarProduct :> sp], #, {All,All}]& //
    Flatten //
    Union;
  sps = Join[Complement[sps, p2s], p2s];
  nSps = Length[sps];
  If[nSps =!= n*(n+1)/2, (
    Message[GetScalarProductRules::badsps, n*(n+1)/2, ps];
    Return[$Failed]
  )];
  eqsConservation = ScalarProduct[#, Total[ps]] == 0& /@ ps;
  eqsExpand = Subtract @@@ (Join[eqs, eqsConservation] // EvaluateScalarProducts);
  eqsMatrix = Outer[Coefficient, eqsExpand, sps];
  rank = MatrixRank[eqsMatrix];
  eqsRHS = Factor[eqsExpand - eqsMatrix.sps];
  eqsRowReduced = RowReduce[Join[eqsMatrix, List/@eqsRHS, 2]];
  If[rank != MatrixRank[eqsRowReduced], (
    Message[GetScalarProductRules::badeqs, sps, Join[eqsConservation, eqs]];
    Return[$Failed]
  )];
  If[rank < nSps,
    Message[GetScalarProductRules::lowrank, rank, nSps]
  ];
  AppendTo[eqsRowReduced, Append[sps, 1]];
  Do[
    If[eqsRowReduced[[i, i]] == 0,
      (
        j = LengthWhile[eqsRowReduced[[i]], #===0&] + 1;
        If[j <= i || j > nSps, (
          Message[GetScalarProductRules::reduce, eqsRowReduced];
          Return[$Failed]
        )];
        (*swap columns*)
        tmp = eqsRowReduced[[All, i]];
        eqsRowReduced[[All, i]] = eqsRowReduced[[All, j]];
        eqsRowReduced[[All, j]] = tmp;
      )
    ],
    {i, rank}
  ];
  eqsMatrix = eqsRowReduced[[;;rank, ;;rank]];
  eqsRHS = Expand[
    Dot[eqsRowReduced[[;;rank, rank + 1;;]], (-1)*eqsRowReduced[[-1, rank+1;;]]]
  ];
  ruleEqs = Thread[eqsRowReduced[[-1, ;;rank]] -> LinearSolve[eqsMatrix, eqsRHS]];
  Return[Sort@Thread[sps->(sps/.ruleEqs)]]
]


End[]


EndPackage[]


fileStamp[]
