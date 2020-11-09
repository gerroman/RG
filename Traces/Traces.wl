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


diracConjugate::usage = "
  diracConjugate[expr] perform conjugation for fermion line
";


spinSum::usage = "
  spinSum[p] perform sum over fermion p spin states
  spinSum[] perform sum over spin states for all found fermions
";

Begin["`Private`"]


id /: Dot[x___, id, y__] := Dot[x, y];
id /: Dot[x__, id, y___] := Dot[x, y];

gamma = \[Gamma];
tr[gamma[\[Mu]_Symbol]] = 0;
tr[expr_Dot] := 0 /; (
  OddQ[Length[List@@expr]]
  && MatchQ[Map[Head, List@@expr], {gamma...}]
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
       ),
       tr[f_* a_] :> f tr[a] /; (
         NumberQ[f] || MatchQ[f, Alternatives@@(powersPattern[traceScalars])]
       )
    }
  ]
];


traceLongRule = (
  tr[Dot[gamma[a_], l__ /; MatchQ[{l}, {_gamma...}]]] :> Plus @@ MapIndexed[
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


diracConjugate = With[
  {
    rule = Conjugate[expr:(Dot[(_bar`u|_bar`v), ___gamma, (_u|_v)])] :> ReplaceAll[
        Reverse[expr],
	{u -> bar`u, v -> bar`v, bar`u -> u, bar`v -> v}
     ]
  },
  ReplaceAll[#, rule] & 
];

spinSum[p_] := With[{
    rules = {
      Dot[a__, u[p]] Dot[bar`u[p], b__] :> Dot[a, (gamma[p] + mass[p] id), b],
      Dot[a__, v[p]] Dot[bar`v[p], b__] :> Dot[a, (gamma[p] - mass[p] id), b],
      Dot[bar`u[p], a___, u[p]] :>  tr[Dot[a, (gamma[p] + mass[p] id)]],
      Dot[bar`v[p], a___, v[p]] :> tr[Dot[a, (gamma[p] - mass[p] id)]]
    }
  },
  ReplaceAll[#, rules] &
];
spinSum[] = With[{
    rules = {
      Dot[a__, u[p_]] Dot[bar`u[p_], b__] :> Dot[a, (gamma[p] + mass[p] id), b],
      Dot[a__, v[p_]] Dot[bar`v[p_], b__] :> Dot[a, (gamma[p] - mass[p] id), b],
      Dot[bar`u[p_], a__, u[p_]] :>  tr[Dot[a, (gamma[p] + mass[p] id)]],
      Dot[bar`v[p_], a__, v[p_]] :> tr[Dot[a, (gamma[p] - mass[p] id)]]
    }
  },
  ReplaceRepeated[#, rules] &
];


End[]


EndPackage[]
