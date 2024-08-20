(* ::Package:: *)


(* ::Title:: *)
(*Diagrams*)


(* ::Text:: *)
(*Draw basic Feynman diagrams with Wolfram Mathematica*)


BeginPackage["RG`Diagrams`"]


drawLine::usage = "
  drawLine[{p1, p2, ...}] draw a line segment from the point p1 and the point p2, rendering is affected by lineDirectives
  drawLine[{p1, p2}, label] \[LongDash] labeled version
  drawLine[{p1, p2, ...}, {label1, ...}] \[LongDash] multi-segment labeled version
";

drawArrow::usage = "
  drawArrow[{p1, p2, ...}] draw a arrow segment from the point p1 and the point p2, rendering is affected by lineDirectives
  drawArrow[{p1, p2}, label] \[LongDash] labeled version
  drawArrow[{p1, p2, ...}, {label1, ...}]
";

drawSpring::usage = "
  drawSpring[{p1, p2, ...}] draw a spring segment from the point p1 and the point p2, rendering is affected by lineDirectives, and springParams
  drawSpring[{p1, p2}, label] \[LongDash] labeled version
";

drawWave::usage = "
  drawWave[{p1, p2, ...}] draw a wave segment from the point p1 and the point p2, rendering is affected by lineDirectives, and springParams
  drawWave[{p1, p2}, label] \[LongDash] labeled version
";

drawLabel::usage = "drawLabel[{p1, p2}, {expr, pos, offset, dir}] draw text at (1 - pos) * p1 + pos * p2
  drawLabel[{p1, p2}, expr] draw text with default pos = 0.5, offset = {0, -2}, dir = {1, 0}
";

lineDirectives::usage = "
  lineDirectives \[LongDash] drawLine and drawArrow option, a list of directives to draw a line/arrow
";

waveParams::usage = "
  waveParams \[LongDash] drawSpring and drawWave option, a list {m, r, n}, where m \[LongDash] spring radius/length ratio, r \[LongDash] number of spring rings, n \[LongDash] total number of points to draw spring
"


drawFrame::usage = "
  drawFrame[{p1x, p1y}, {p2x, p2y}] draw rectangular frame with p1, p2 as opposite corners
  drawFrame[d:1] draw square frame with d as side length
"


Begin["`Private`"]



Options[drawLine] = {lineDirectives -> {}};
drawLine[ps:{{_,_}...}, OptionsPattern[]] := Append[
  Flatten[{OptionValue[lineDirectives]}],
  Line[ps]
];

drawLabel[ps:{p1:{_,_}, p2:{_,_}}, {expr_, pos_:0.5, offset_:{0, -2}, dir_:{1, 0}}] := (
  Text[expr, (1 - pos) * p1 + pos * p2, offset, dir]
);
drawLabel[ps:{p1:{_,_}, p2:{_,_}}, expr_] := drawLabel[ps, {expr}];
drawLabel[p_List /; VectorQ[p], expr_] := drawLabel[{p, p}, expr];

drawLine[ps:{p1:{_,_}, p2:{_,_}}, {label:{__}}, opts:OptionsPattern[]] := Append[
  drawLine[ps, opts],
  drawLabel[ps, label]
];

drawLine[ps:{p1:{_,_}, p2:{_,_}}, expr_, opts:OptionsPattern[]] := drawLine[ps, {{expr}}, opts];

drawLine[ps:{p1:{_,_}, p2:{_,_}}, l_List, opts:OptionsPattern[]] := Join[
  drawLine[ps, opts],
  drawLabel[ps, #]& /@ l
];

drawLine[ps:{{_,_}..}, labels_List, opts:OptionsPattern[]] := (
  Map[drawLine[#[[1]], #[[2]], opts]&, Transpose[{Partition[ps, 2, 1], labels}]]
) /; Length[ps] == Length[labels] + 1;


Options[drawArrow] = {lineDirectives -> {}};
drawArrow[ps:{{_,_}, {_,_}}, OptionsPattern[]] := Append[
  Flatten[{Arrowheads[{{Automatic, 0.5}}], OptionValue[lineDirectives]}],
  Arrow[ps]
];

drawArrow[ps:{p1:{_,_}, p2:{_,_}}, {label:{__}}, opts:OptionsPattern[]] := Append[
  drawArrow[ps, opts],
  drawLabel[ps, label]
];

drawArrow[ps:{p1:{_,_}, p2:{_,_}}, expr_, opts:OptionsPattern[]] := drawArrow[ps, {{expr}}, opts];

drawArrow[ps:{p1:{_,_}, p2:{_,_}}, l_List, opts:OptionsPattern[]] := Join[
  drawArrow[ps, opts],
  drawLabel[ps, #]& /@ l
];

drawArrow[ps:{{_,_}..}, labels_List, opts:OptionsPattern[]] := (
  Map[drawArrow[#[[1]], #[[2]], opts]&, Transpose[{Partition[ps, 2, 1], labels}]]
) /; Length[ps] == Length[labels] + 1;

drawArrow[ps:{{_,_}..}, opts:OptionsPattern[]] := (
  Map[drawArrow[#, opts]&, Partition[ps, 2, 1]]
);


Options[drawSpring] = {lineDirectives -> {}, waveParams -> {0.05, 5, 100}}
drawSpring[{p1 : {_, _}, p2 : {_, _}}, opts:OptionsPattern[]] := Module[{
    d = Norm[1.0 * (p2 - p1)],
    rot = RotationTransform[Last @ ToPolarCoordinates[p2 - p1]],
    m, r, n, points
  },
  {m, r, n} = OptionValue[waveParams];
  points = Map[
    p1 + rot[#] &,
    Table[
      With[{
          x = 2.0 * Pi * (r + 1 / 2) * (i / n)
        },
        {d, 0} * (i / n)
        + d * m * {1  - 2 * (i / n) - Cos[x], Sin[x]}
      ],
      {i, 0, n}
    ]
  ];
  Append[
    Flatten[{OptionValue[lineDirectives]}],
    Line[points]
  ]
];

drawSpring[ps:{p1 : {_, _}, p2 : {_, _}}, {label:{__}}, opts:OptionsPattern[]] := Append[
  drawSpring[ps, opts],
  drawLabel[ps, label]
];

drawSpring[ps:{p1 : {_, _}, p2 : {_, _}}, expr_, opts:OptionsPattern[]] := drawSpring[ps, {{expr}}, opts];

drawSpring[ps:{p1:{_,_}, p2:{_,_}}, l_List, opts:OptionsPattern[]] := Join[
  drawSpring[ps, opts],
  drawLabel[ps, #]& /@ l
];

drawSpring[ps:{{_,_}..}, labels_List, opts:OptionsPattern[]] := (
  Map[drawSpring[#[[1]], #[[2]], opts]&, Transpose[{Partition[ps, 2, 1], labels}]]
) /; Length[ps] == Length[labels] + 1;


Options[drawWave] = {lineDirectives -> {}, waveParams -> {0.05, 5, 100}}
drawWave[{p1 : {_, _}, p2 : {_, _}}, opts:OptionsPattern[]] := Module[{
    d = Norm[1.0 * (p2 - p1)],
    rot = RotationTransform[Last @ ToPolarCoordinates[p2 - p1]],
    m, r, n, points
  },
  {m, r, n} = OptionValue[waveParams];
  points = Map[
    p1 + rot[#] &,
    Table[
      With[{
          x = 2.0 * Pi * (r + 1 / 2) * (i / n)
        },
        {d, 0} * (i / n)
        + d * m * {0, Sin[x]}
      ],
      {i, 0, n}
    ]
  ];
  Append[
    Flatten[{OptionValue[lineDirectives]}],
    Line[points]
  ]
];

drawWave[ps:{p1 : {_, _}, p2 : {_, _}}, {label:{__}}, opts:OptionsPattern[]] := Append[
  drawWave[ps, opts],
  drawLabel[ps, label]
];

drawWave[ps:{p1 : {_, _}, p2 : {_, _}}, expr_, opts:OptionsPattern[]] := drawWave[ps, {{expr}}, opts];

drawWave[ps:{p1:{_,_}, p2:{_,_}}, l_List, opts:OptionsPattern[]] := Join[
  drawWave[ps, opts],
  drawLabel[ps, #]& /@ l
];

drawWave[ps:{{_,_}..}, labels_List, opts:OptionsPattern[]] := (
  Map[drawWave[#[[1]], #[[2]], opts]&, Transpose[{Partition[ps, 2, 1], labels}]]
) /; Length[ps] == Length[labels] + 1;


drawFrame[p1 : {xmin_, ymin_}, p2 : {xmax_, ymax_}] := Graphics[
  {Opacity[.0], Rectangle[p1, p2]},
  Frame -> True,
  FrameTicks -> {Range[xmin, xmax], Range[ymin, ymax]},
  GridLines -> {Range[xmin, xmax], Range[ymin, ymax]},
  GridLinesStyle -> Directive[Opacity[.3], Dashed]
];

drawFrame[d_: 1] := drawFrame[{-d, -d}, {d, d}];


End[]


(* Print[$Context]; *)


EndPackage[]
