(* ::Package:: *)

(* ::Title:: *)
(*FeynmanDiagrams*)


(* ::Text:: *)
(*Graphics primitives to draw basic Feynman diagrams*)


BeginPackage["RG`FeynmanDiagrams`"]


electronLine::usage = "
  electronLine[{p1, p2, ...}, {label1, label2, ...}, flip, arrowWidth] draw electron line
"

photonLine::usage = "
  photonLine[{p1, p2}, label, nWiggles, flip, arrowWidth, drawArrow] draw photon line
"

electronArc::usage = "
  electronArc[{p1, p2}, label, arcAngle, flip, arrowWidth] draw electron arc line
"

photonArc::usage = "
  photonArc[{p1, p2}, label, nWiggles, arcAngle, flip, arrowWidth] draw photon arc line
"


particleLine::usage = "
  particleLine[{{x, y}, ...}, label, {lineOpts}, {labelOpts}, flip, arrowWidth, shiftFactors]
";


Begin["`Private`"]


(* ::Section:: *)
(*Electron line*)


getLength[{p1:{_, _}, p2:{_, _}}] := (p1 - p2) // Norm;
getCrossDirection[{p1:{_, _}, p2:{_, _}}] := (p1 - p2) // Reverse // ({1, -1} * # &) // Normalize;
getMiddlePosition[{p1:{_, _}, p2:{_, _}}] := (p1 + p2) / 2;
getFlipFactor[b_?BooleanQ] := 2Boole[b] - 1;


electronLine[points:{{_,_}...}, labels_List, flip_:False, arrowWidth_:Automatic, shiftFactors_:2] := Module[{
    n = Length[points]
    , subsequentPoints = Subsequences[N[points],{2}]
    , lengths
    , crossDirections
    , flipFactors
    , arrowHeadsPositions
    , labelPositions
  },
  lengths = Map[getLength, subsequentPoints];
  crossDirections = Map[getCrossDirection, subsequentPoints];
  flipFactors = If[BooleanQ[flip], ConstantArray[getFlipFactor[flip], n - 1], Map[getFlipFactor, flip]];
  arrowHeadsPositions = ((* subsequent middles as fractures of total length *)
    FoldList[{#1[[2]] + #2 / 2, #1[[2]] + #2}&, {0, 0}, lengths] // Map[First] // Rest
  ) / Total[lengths];
  labelPositions = Map[getMiddlePosition, subsequentPoints];
  Graphics[{
    {
      Arrowheads[Map[{arrowWidth, #}&, arrowHeadsPositions]],
      Arrow[points]
    },
    Thread[Text[
      Map[StringForm["``", #]&, labels],
      labelPositions,
      shiftFactors * flipFactors * crossDirections
    ]]
  }]
];


(* ::Section:: *)
(*Photon line*)


photonLine[points:{p1:{_,_}, p2:{_,_}}, label_, nWiggles_:4, flip_:False, arrowWidth_:Automatic, shiftFactor_:2, drawArrow_:True] := Module[{
    direction = N@(p2 - p1) // Normalize
    , segmentLength = getLength[N@points] / nWiggles
    , crossDirection = getCrossDirection[N@points] * getFlipFactor[flip]
    , bezierPoints
  },
  bezierPoints = Array[n \[Function] {
    p1 + direction * (n - 1/2) * segmentLength + crossDirection * segmentLength / 2,
    p1 + direction * (n - 1/2) * segmentLength - crossDirection * segmentLength / 2,
    p1 + direction * n * segmentLength
  }, nWiggles] // Flatten[#, 1]& // Prepend[#, p1]&;
  Graphics[{
    BezierCurve[bezierPoints],
    With[{pos=getMiddlePosition[N@points]}, {
    If[drawArrow,
      {
        Arrowheads[arrowWidth],
        Arrow[{
	      pos - direction * segmentLength / 2 - crossDirection * segmentLength / 2,
	      pos + direction * segmentLength / 2 - crossDirection * segmentLength / 2
        }]
      }, Nothing],
      Text[label, pos - crossDirection * segmentLength / 2, shiftFactor*crossDirection]
    }]
  }]
];


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

  Graphics[{
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
  }]
];
(*  *)


(* ::Section:: *)
(*Electron arc*)


electronArc[points:{{_,_},{_,_}}, label_, arcAngle_:\[Pi], flip_:False, arrowWidth_:Automatic, shiftFactor_:2] := Module[{
    fullSegmentLength, direction, crossDirection, radius, centerPosition, initialAngle, finalAngle,
    segmentAngle, segmentLength
  },
  {fullSegmentLength, direction, crossDirection, radius, centerPosition, initialAngle, finalAngle,
    segmentAngle, segmentLength} = arcHelper[points, 1, arcAngle, flip];
  Graphics[{
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
  }]
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
			el = First@electronLine[points, {label}, flip, arrowWidth, shiftFactors]
		},
		With[{
				line = Flatten[{opts1}]~Join~Rest[First[el]],
				text = MapAt[Flatten[Style[#, opts2]] &, el[[2]], {1}]
			},
			{line, text}
		]
	];


End[]


line`electron = electronLine;
line`photon = photonLine;
arc`electron = electronArc;
arc`photon = photonArc;


Print[$Context];


EndPackage[]
