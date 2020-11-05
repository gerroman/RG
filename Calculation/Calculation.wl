(* ::Package:: *)

(* ::Text:: *)
(*Functions to perform routine transformations*)


BeginPackage["RG`Calculation`"];


modify::usage = "
  modify[pattern, fs] create function to replace all matches of the pattern to results of consequent application of functions fs to these matches
  modify[{x1, ...}, fs] create function for specific x1, ...
";


pullFactors::usage = "
  pullFactors[pattern, func] pull factors x from func which match func[___, (x:pattern) * ___, ___]
  pullFactors[{x1,...}, func] pull concrete xs
";


groupIt::usage = "
  groupIt[expr, func] find and group expr modified by functions func
";


fixedPoint::usage = "
  fixedPoint[func] is shortcut for FixedPoint[#, func] &
";


release::usage = "
  release[expr] apply ReleaseHold repeatedly
";


factorIt::usage = "
  factorIt[pattern, modifier] factor out factors matches pattern apply modifier to the rest
  factorIt[{x1, ...}, modifier] factor out concrete xs
";
pullIt::usage = "
  pullIt[pattern, modifier] pull out factors matches pattern apply modifier to the rest
  pullIt[{x1, ...}, func] pull out concrete xs
";


powersPattern::usage = "
  powersPattern[{x1, ...}] return patterns for all possible powers of xs
";


rewriteIt::usage = "
  rewrite[eq, func] rewrite equation applying func to the right hand side
";


Begin["`Private`"];


modify[xs_List, fs_List] := With[
   {rules = Thread[Rule[xs, Map[RightComposition@@fs, xs]]]},
   ReplaceAll[rules]
];
modify[pattern_, fs_List] := Function[expr,
  With[{xs = Union@Cases[{expr}, pattern, Infinity]},
    modify[xs, fs][expr]
  ]
];
modify[expr_, fs__] := modify[expr, {fs}];


pullFactors[arg_, func_, maxIter_:$IterationLimit] := With[{
    pattern = If[Head[arg] === List,
      If[Length[arg] == 1, First@arg, Alternatives@@arg],
      arg
    ]
  },
  Function[{expr},
    FixedPoint[
      ReplaceAll[func[a___, (x:pattern) * b_, c___] :> x func[a, b, c]],
      expr,
      maxIter
    ]
  ]
];


fixedPoint[func_, args___] := FixedPoint[func, #, args]&;


release = fixedPoint[ReleaseHold];


groupIt[expr_, func_:Expand] := ReplaceAll[(expr // modify[{expr}, func]) -> expr];


factorIt[xs_List, modifier_:Identity, func_:Plus, maxIter_:$IterationLimit] := With[{
    rules = Map[
      {
        func[# * a_., # * b_.] :> # modifier[func[a, b]],
        func[# * a_., -# * b_.] :> # modifier[func[a, -b]]
      }&,
      xs
    ] // Flatten
  },
  FixedPoint[ReplaceAll[rules], #, maxIter] &
];
factorIt[pattern_, modifier_:Identity, func_:Plus, maxIter_:$IterationLimit] := With[{
    rules = {
        func[(x:pattern) * a_., x_ * b_.] :> x modifier[func[a, b]]
      }
  },
  FixedPoint[ReplaceAll[rules], #, maxIter] &
];


pullIt[xs_List, modifier_:Identity, func_:Plus, maxIter_:$IterationLimit] := With[{
    rules = Map[
      x \[Function] {
        func[a__, x * b_., c___] :> x modifier[Map[ (# / x) &, func[a, x b, c]]],
        func[a___, x * b_., c__] :> x modifier[Map[ (# / x) &, func[a, x b, c]]]
      },
      xs
    ] // Flatten
  },
  FixedPoint[ReplaceAll[rules], #, maxIter] &
];
pullIt[pattern_, modifier_:Identity, func_:Plus, maxIter_:$IterationLimit] := With[{
    rules = {
        func[a__, (x:pattern) * b_., c___] :> x modifier[Map[ (# / x) &, func[a, x b, c]]],
        func[a___, (x:pattern) * b_., c__] :> x modifier[Map[ (# / x) &, func[a, x b, c]]]
      }
  },
  FixedPoint[ReplaceAll[rules], #, maxIter] &
];

powersPattern[xs_List] := Subsets[xs] // Reverse //
  Map[#^_. &, #, {2}] & //
  Apply[Times, #, {1}] & // 
  PowerExpand // ReplaceAll[x_ y_Optional :> y];



rewriteIt[Equal[lhs_, rhs_], func_] := Equal[lhs, func[rhs]]


End[];


EndPackage[];
