BeginPackage["RG`DrawGraph`"]


DrawGraph::usage="DrawGraph[g, vpos] draw graph using vertex positions"


fermionLine::usage = "fermionLine[{p1, p2, ...}] electron line"
photonLine::usage = "photonLine[{p1, p2}] photon line"
fermionArc::usage = "fermionArc[{p1, p2}] fermion arc line"
photonArc::usage = "photonArc[{p1, p2}] photon arc line"


Begin["`Private`"]


getLength[{p1:{_, _}, p2:{_, _}}] := (p1 - p2) // Norm;
getCrossDirection[{p1:{_, _}, p2:{_, _}}] := (p1 - p2) // Reverse // ({1, -1} * # &) // Normalize;
getMiddlePosition[{p1:{_, _}, p2:{_, _}}] := (p1 + p2) / 2;


Options[fermionLine] = {
  "ArrowSize"->Automatic,
  "ArrowShift"->0.
}

fermionLine[points:{{_, _}...}, opt:OptionsPattern[]] := Module[{
    subsequentPoints = Subsequences[points, {2}],
    shift,
    ls,
    l
  },
  ls = Map[getLength, subsequentPoints];
  l = Total[ls];
  shift = OptionValue["ArrowShift"];
  arrowHeadsPositions = (
    (* subsequent middles as fractures of total length *)
    FoldList[
      {#1[[2]] + #2 (1 + shift) / 2, #1[[2]] + #2}&,
      {0, 0},
      ls
    ] // Map[First] // Rest
  ) / l;
  {
    Arrowheads[Map[{OptionValue["ArrowSize"], #}&, arrowHeadsPositions]],
    Arrow[points]
  }
];


DrawGraph::vertexlist="insufficient vertex position list\n[needed]: ``\n[defined]: ``"
DrawGraph[g_Graph, vsPos_List, opts : OptionsPattern[]] := Module[{
    vs = VertexList[g],
    edges = EdgeList[g],
    ps,
    arrows,
    waves,
    fermionLineOpts=FilterRules[{opts}, Options[fermionLine]],
    graphicsOpts=FilterRules[{opts}, Options[Graphics]],
    photonLineOpts=FilterRules[{opts}, Options[photonLine]],
    aspectRatio,
    graph
  },
  If[Not@ContainsAll[First/@vsPos, vs], (
    Message[DrawGraph::vertexlist, vs, First/@vsPos];
    Return[$Failed];
  )];
  ps = Point[vs /. vsPos];
  arrows = edges // Cases[_DirectedEdge] // ReplaceAll[
      DirectedEdge[v1_, v2_] :> fermionLine[{v1, v2} /. vsPos, fermionLineOpts]
  ];
  waves = edges // Cases[_UndirectedEdge] // ReplaceAll[UndirectedEdge[v1_, v2_] :> photonLine[{v1, v2} /. vsPos, photonLineOpts]];
  aspectRatio = With[{xs=Map[First, First[ps]], ys=Map[Last, First[ps]]},
    With[{dy = Max[ys]-Min[ys], dx=Max[xs] - Min[ys]},
      If[dx == 0 || dy == 0,
        Automatic,
        dy/dx
      ]
    ]
  ];
  graph = Graphics[{arrows, waves}, graphicsOpts, AspectRatio->aspectRatio];
  Return[graph];
]

Options[photonLine] = {
  "DefaultSegmentLength"->0.3,
  "NumberOfWiggles"->Automatic
}

photonLine[points:{p1:{_,_}, p2:{_,_}}, opts:OptionsPattern[]] := Module[{
    direction = N@(p2 - p1) // Normalize,
    l = getLength[N@points],
    segmentLength,
    crossDirection = getCrossDirection[N@points],
    nWiggles,
    bezierPoints,
    graph
  },
  nWiggles = If[OptionValue["NumberOfWiggles"] === Automatic,
    Ceiling[l / OptionValue["DefaultSegmentLength"]]
    ,
    OptionValue["NumberOfWiggles"]
  ];
  segmentLength = l / nWiggles;
  bezierPoints = Array[Function[n, {
    p1 + direction * (n - 1/2) * segmentLength + crossDirection * segmentLength / 2,
    p1 + direction * (n - 1/2) * segmentLength - crossDirection * segmentLength / 2,
    p1 + direction * n * segmentLength
  }], nWiggles] // Flatten[#, 1]& // Prepend[#, p1]&;
  graph = BezierCurve[bezierPoints];
  Return[graph]
];


(*

(* ::Section:: *)
(*Photon arc*)


arcHelper[points:{p1:{_,_},p2:{_,_}}, nWiggles_:4, arcAngle_:\[Pi], flip_:False] := Module[
  {fullSegmentLength, direction, crossDirection, radius, centerPosition, initialAngle, finalAngle, segmentAngle, segmentLength},
  fullSegmentLength = getLength[N@points];
  direction = N@(p2 - p1) // Normalize;
  crossDirection = getCrossDirection[N@points] * getFlipFactor[flip];
  radius = fullSegmentLength /(2 Sin[arcAngle/2]);
  centerPosition = getMiddlePosition[points] + crossDirection * radius * Cos[arcAngle/2];
  initialAngle = ToPolarCoordinates[p1 - centerPosition] // Last;
  finalAngle = ToPolarCoordinates[p2 - centerPosition] // Last;
  segmentAngle = Which[
    arcAngle == \[Pi], getFlipFactor[flip] * arcAngle / nWiggles,
    -\[Pi] < finalAngle - initialAngle < \[Pi], (finalAngle - initialAngle) / nWiggles,
    finalAngle - initialAngle < -\[Pi], (2\[Pi] + finalAngle - initialAngle) / nWiggles,
    finalAngle - initialAngle > \[Pi], (finalAngle - initialAngle - 2\[Pi]) / nWiggles,
    True, (Print["Error in photon arc segment angle calculation"]; Return[None];)
  ];
  segmentLength = (radius * arcAngle) / nWiggles;
  {fullSegmentLength, direction, crossDirection, radius, centerPosition, initialAngle, finalAngle, segmentAngle, segmentLength}
];


photonArc[points:{p1:{_,_},p2:{_,_}}, label_, nWiggles_:4, arcAngle_:\[Pi], flip_:False, arrowWidth_:Automatic, shiftFactor_:2] := Module[{
     fullSegmentLength, direction, crossDirection, radius, centerPosition, initialAngle, finalAngle, segmentAngle, segmentLength,
     bezierPoints
  },
  {fullSegmentLength, direction, crossDirection, radius, centerPosition, initialAngle, finalAngle, segmentAngle, segmentLength} = arcHelper[points, nWiggles, arcAngle, flip];

  bezierPoints = With[{angle = i \[Function] initialAngle + (i - 1/2) * segmentAngle},
    Array[n \[Function] {
      centerPosition + (radius + segmentLength/2)*{Cos[angle[n]], Sin[angle[n]]},
      centerPosition + (radius - segmentLength/2)*{Cos[angle[n]], Sin[angle[n]]},
      centerPosition + radius * {Cos[initialAngle + n * segmentAngle], Sin[initialAngle + n * segmentAngle]}
    }, nWiggles]
  ] // Flatten[#, 1]& // Prepend[#, p1]&;

  {
    BezierCurve[bezierPoints],
    With[{
        pos = centerPosition + radius * {Cos[initialAngle + nWiggles / 2 * segmentAngle], Sin[initialAngle + nWiggles / 2 * segmentAngle]}
      },
      {
        Text[label, pos - crossDirection * segmentLength /2 , shiftFactor * crossDirection],
        {
        Arrowheads[arrowWidth],
        Arrow[{
          pos - direction * segmentLength / 2 - crossDirection * segmentLength /2,
          pos + direction * segmentLength / 2 - crossDirection * segmentLength /2
        }]
      }
      }
    ]
  }
];
(*  *)


(* ::Section:: *)
(*Fermion arc*)


fermionArc[points:{{_,_},{_,_}}, label_, arcAngle_:\[Pi], flip_:False, arrowWidth_:Automatic, shiftFactor_:2] := Module[{
    fullSegmentLength, direction, crossDirection, radius, centerPosition, initialAngle, finalAngle,
    segmentAngle, segmentLength
  },
  {fullSegmentLength, direction, crossDirection, radius, centerPosition, initialAngle, finalAngle,
    segmentAngle, segmentLength} = arcHelper[points, 1, arcAngle, flip];
  {
    Circle[centerPosition, radius, {initialAngle, initialAngle+segmentAngle}],
    With[{pos = centerPosition + radius * {Cos[initialAngle + segmentAngle / 2], Sin[initialAngle + segmentAngle / 2]}},
      {
        Text[label, pos, shiftFactor * crossDirection],
        {
          Arrowheads[arrowWidth],
        Arrow[{pos - direction * segmentLength / 100, pos + direction * segmentLength / 100}] (* small arrow *)
        }
      }
    ]
  }
];

(*  *)


particleLine[
    points : {{_, _}, {_, _}}
    , label_
    , opts1_: {}
        , opts2_: {}
    , flip_: False
    , arrowWidth_: Automatic
    , shiftFactors_: 2
  ] :=
  Module[{
      el = First@fermionLine[points, {label}, flip, arrowWidth, shiftFactors]
    },
    With[{
        line = Flatten[{opts1}]~Join~Rest[First[el]],
        text = MapAt[Flatten[Style[#, opts2]] &, el[[2]], {1}]
      },
      {line, text}
    ]
  ];

*)


End[]


EndPackage[]
