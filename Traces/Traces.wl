(* ::Package:: *)

(* ::Text:: *)
(*Functions to perform trace calculations*)


BeginPackage["RG`Traces`", {
  "RG`Notation`",
  "RG`Calculation`",
  "RG`Kinematics`"
}]


tr::usage = "
  tr[expr] represents trace of expr
";


traceScalars::usage = "
  traceScalars = {...} set scalars to pull out of traces
";
traceCalc::usage = "
  traceCalc[expr] calculate trace of \[Gamma] matrices dot product
";
contractLorentzIndices::usage = "
  contractLorentzIndices[indexes][expr] contracts free Lorentz indices
";


Begin["`Private`"]


id /: Dot[x___, id, y__] := Dot[x, y];
id /: Dot[x__, id, y___] := Dot[x, y];


tr[\[Gamma][\[Mu]_Symbol]] = 0;
tr[expr_Dot] := 0 /; (
  OddQ[Length[List@@expr]]
  && MatchQ[Map[Head, List@@expr], {\[Gamma]...}]
  && Not@MemberQ[Map[First, List@@expr], _Integer]
);


traceScalars = {};
pullTraceScalars = Function[expr,
  ReplaceRepeated[
    expr,
    {
       tr[Dot[a___, b_ * f_, c___]] :>
       f tr[Dot[a, b, c]] /; (
         NumberQ[f] || MatchQ[f, Alternatives@@(powersPattern[traceScalars])]
       )
    }
  ]
];


traceLongRule = (
  tr[Dot[\[Gamma][a_], l__ /; MatchQ[{l}, {_\[Gamma]...}]]] :> Plus @@ MapIndexed[
    (-(-1)^First[#2] sp[a, First[#1]] tr[Dot[id, Dot@@(Drop[{l}, #2])]])&,
    {l}
  ]
);


traceCalc := FixedPoint[(
  modify[_Dot, Distribute] /*
  modify[_tr, Distribute] /*
  pullTraceScalars /*
  (ReplaceRepeated[#, {tr[id] -> 4, traceLongRule}] &) /*
  Expand
), #] &;


contractLorentzIndices[indices_List] := With[
  {ids = Alternatives@@indices},
  Function[expr,
    ReplaceRepeated[expr, {
      sp[a_, i:ids] * sp[b_, i:ids] :> sp[a, b],
      sp[a_, i:ids]^2 :> sp[a, a],
      sp[i:ids, i:ids] -> 4
    }]
  ]
];
contractLorentzIndices[indices__] := contractLorentzIndices[{indices}];


End[]


EndPackage[]
