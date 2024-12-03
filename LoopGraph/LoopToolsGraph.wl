BeginPackage["RG`LoopGraph`LoopToolsGraph`", {"RG`Kinematics`ScalarInvariants`"}]


LoopToolsGraph::usage="LoopToolsGraph[expr, styleRule:{}] \[LongDash] draws LoopTools expressions as graphs"


pattern`LTscalar = _LoopTools`A0|_LoopTools`B0|_LoopTools`C0|_LoopTools`D0|_LoopTools`E0|_LoopTools`F0
pattern`LTtensor = _LoopTools`A0i|_LoopTools`B0i|_LoopTools`C0i|_LoopTools`D0i|_LoopTools`E0i|_LoopTools`F0i|pattern`LTscalar
pattern`LTonePoint = _LoopTools`A0i|_LoopTools`A0
pattern`LTtwoPoint = _LoopTools`B0i|_LoopTools`B0
pattern`LTthreePoint = _LoopTools`C0i|_LoopTools`C0
pattern`LTfourPoint = _LoopTools`D0i|_LoopTools`D0
pattern`LTfivePoint = _LoopTools`E0i|_LoopTools`E0
pattern`LTsixPoint = _LoopTools`F0i|_LoopTools`F0


Begin["Private`"];


LoopToolsGraph[expr:pattern`LTtensor, styleRule_List:{}] := Module[
  {
    n, args, m2s, invariants, loopLabels, legsLabels, ps, ms
  },
  n = Which[
    MatchQ[expr, pattern`LTonePoint], 1,
    MatchQ[expr, pattern`LTtwoPoint], 2,
    MatchQ[expr, pattern`LTthreePoint], 3,
    MatchQ[expr, pattern`LTfourPoint], 4,
    MatchQ[expr, pattern`LTfivePoint], 5,
    MatchQ[expr, pattern`LTsixPoint], 6,
    True, Return[$Failed]
  ];
  args = Which[
    MatchQ[expr, pattern`LTscalar], List@@expr,
    True, Rest[List@@expr]
  ];
  m2s = args[[-n;;]];
  ps = Array[Subscript["p", #]&, n];
  ms = Array[Subsuperscript["m", #, 2]&, n, 0];
  invariants={};
  If[n > 1,
    invariants = Thread[GetScalarInvariants[ps, Times] == args[[;;-n-1]]];
  ];
  If[n == 2,
    invariants=Join[invariants, invariants]
  ];
  loop = Inner[DirectedEdge, RotateRight[Range[n]], Range[n], List];
  legs = Inner[DirectedEdge, -Range[n], Range[n], List];
  loopLabels = loop // Map[Replace[edge:DirectedEdge[i_, j_]:>(edge->(ms[[j]] == m2s[[j]]))]];
  If[n == 1,
    legsLabels = {},
    legsLabels = legs // Map[Replace[edge:DirectedEdge[i_, j_]:>(edge->invariants[[j]])]]
  ];
  Labeled[
    Graph[
      Join[loop, legs],
      EdgeLabels->Join[loopLabels, legsLabels]
    ]
    ,
    Prepend[If[n>=4, invariants[[n+1;;]], {}], expr]
  ]
]


End[]


EndPackage[]


Print[ToString@StringForm["[info]: `` loaded", $InputFileName]]
