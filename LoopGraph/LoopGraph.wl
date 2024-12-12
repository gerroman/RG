BeginPackage["RG`LoopGraph`"]


LoopGraph::usage="LoopGraph[l, ds] \[LongDash] graph defined by loop momenta, l, and propagator denominators, ds. It is assumed that the denominators has a form of m2 - (l + pi)^2"

rule`LoopGraphTags::usage="rule`LoopGraphTags \[LongDash] rule to substitute tags";


Begin["`Private`"]


pattern`denominator[l_] := (_. - _[l + _., ___]|_. - _[-l + _., ___])
ruleLinear[f_] := {
  f[a___, b_Integer c_, d___] :> b f[a, c, d]
  ,
  f[a___, b_Plus, d___] :> Distribute[f[a,b,d]]
}

rule`LoopGraphTags = Association[{_->Gray}];


LoopGraph::denominator="denominators `` are not match pattern ``"
Options[LoopGraph] = {
  "colorize"->True,
  EdgeStyle->Automatic,
  ImageSize->Medium,
  EdgeLabeling->True
}

LoopGraph[l_Symbol, ds_List, opts:OptionsPattern[]] := Module[{
    n = Length[ds],
    ps, m2s,
    loop, legs, edges, tags, labels,
    loopLabels, legsLabels, sps, sp, edgeStyle, edgeLables,
    colors=ColorData[63, "ColorList"]
  },
  If[Not@MatchQ[ds, {pattern`denominator[l]..}], (
    Message[LoopGraph::denominator, ds, pattern`denominator[l]];
    Return[$Failed];
  )];
  {ps, m2s, sps} = Transpose[
    Map[
      Replace[{
        (m2_. - expr:(_[l + p_., ___])) :> {p, m2, expr}
        ,
        (m2_. - expr:(_[-l + p_., ___])) :> {-p, m2, expr}
      }]
      ,
      ds
    ]
  ];
  sp = Head[First[sps]];
  ps = RotateLeft[ps] - ps; (* p[2] - p[1], p[3] - p[2], ..., p[1] - p[n] *)
  loop = Inner[DirectedEdge, RotateRight[Range[n]], Range[n], List];
  legs = Inner[DirectedEdge, -Range[n], Range[n], List];
  loopLabels = Map[
    Replace[edge:DirectedEdge[i_, j_] :> (
      edge -> If[OptionValue["colorize"],
        {ds[[j]], m2s[[j]]},
        ds[[j]]
      ]
    )]
    ,
    loop
  ];
  legsLabels = Map[
    Replace[edge:DirectedEdge[i_, j_] :> (
      edge -> If[OptionValue["colorize"],
        {ps[[j]], sp[ps[[j]], ps[[j]]] //. ruleLinear[sp]},
        ps[[j]]
      ]
    )]
    ,
    legs
  ];
  edges = Join[loop, legs];
  edgeLabels = Join[loopLabels, legsLabels];
  labels = Join[loopLabels, legsLabels];
  edgeStyle = OptionValue[EdgeStyle];
  If[OptionValue["colorize"], (
    tags = Union[Map[Last[Last[#]]&, labels]];
    Scan[If[Not[KeyExistsQ[rule`LoopGraphTags, #]],
      AssociateTo[
        rule`LoopGraphTags,
        #->colors[[Mod[Length[rule`LoopGraphTags], Length[colors]]]]
      ]]&
      ,
      tags
    ];
    edgeStyle = Map[
      Replace[(edge_ -> {_, tag_}) :> (edge -> (tag/.rule`LoopGraphTags))]
      ,
      labels
    ];
  )];
  Graph[
    edges,
    EdgeLabels->If[OptionValue[LoopGraph, EdgeLabeling], edgeLabels, None],
    EdgeStyle->edgeStyle,
    ImageSize->OptionValue[LoopGraph, ImageSize]
  ]
]


End[]


EndPackage[]


Print[ToString@StringForm["[info]: `` loaded", $InputFileName]]
