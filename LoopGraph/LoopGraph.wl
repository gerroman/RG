BeginPackage["RG`LoopGraph`"]


LoopGraph::usage="LoopGraph[l, ds] \[LongDash] graph defined by loop momenta, l, and propagator denominators, ds. It is assumed that the denominators has a form of m2 - (l + pi)^2"


Begin["`Private`"]


pattern`denominator[l_] := (_. - _[l + _., ___])


LoopGraph::denominator="denominators `` are not match pattern ``"
LoopGraph[l_Symbol, ds_List] := Module[{
    n = Length[ds],
    ps, m2s,
    loop, legs,
    loopLabels, legsLabels, sps
  },
  If[Not@MatchQ[ds, {pattern`denominator[l]..}], (
    Message[LoopGraph::denominator, ds, pattern`denominator[l]];
    Return[$Failed];
  )];
  {ps, m2s, sps} = Transpose[Map[Replace[(m2_. - expr:(_[l + p_., ___])) :> {p, m2, expr}], ds]];
  ps = RotateLeft[ps] - ps; (* p[2] - p[1], p[3] - p[2], ..., p[1] - p[n] *)
  loop = Inner[DirectedEdge, RotateRight[Range[n]], Range[n], List];
  legs = Inner[DirectedEdge, -Range[n], Range[n], List];
  loopLabels = loop // Map[Replace[edge:DirectedEdge[i_, j_]:>(edge->ds[[j]])]];
  legsLabels = legs // Map[Replace[edge:DirectedEdge[i_, j_]:>(edge->ps[[j]])]];
  Graph[
    Join[loop, legs],
    EdgeLabels->Join[loopLabels, legsLabels]
  ]
]


End[]


EndPackage[]


Print[ToString@StringForm["[info]: `` loaded", $InputFileName]]
