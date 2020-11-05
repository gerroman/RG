(* ::Package:: *)

(* ::Text:: *)
(*Definitions and functions to work with 4 vectors and invariants*)


BeginPackage["RG`Kinematics`", {
  "RG`Notation`", (* energy, mass, momentum, sp, ...*)
  "RG`Calculation`" (* modify *)
}];


replaceEnergy::usage = "
  replaceEnergy[p] replace energy[p]^2 to (abs@momentum[p])^2 + mass[p]^2
  replaceEnergy[p, All] also replace energy[p] to Sqrt[(abs@momentum[p])^2 + mass[p]^2]
";

replaceMomentum::usage = "
  replaceMomentum[p] replace (abs@momentum[p])^2  to energy[p]^2 - mass[p]^2
  replaceMomentum[p, All] also replace (abs@momentum[p])^2  to Sqrt[energy[p]^2 - mass[p]^2]
";

replaceMass::usage = "
  replaceMass[p] replace (mass[p])^2 to energy[p]^2 - (abs@momentum[p])^2
  replaceMass[p, All] replace (mass[p])^2 to Sqrt[energy[p]^2 - (abs@momentum[p])^2]
";


setInvariants::usage = "
  setInvariants[ps, ms, pRules, spRules] set up all scalar products 
";


Begin["`Private`"];


sp[expr_] := sp[expr, expr];
sp[a___, b_ * mult_?NumberQ, c___] := mult * sp[a, b, c];


energy[expr_Plus] := energy /@ expr;
energy[expr_ * factor_?NumberQ] := factor * energy[expr];

momentum[expr_Plus] := momentum /@ expr;
momentum[expr_ * factor_?NumberQ] := factor * momentum[expr];


abs /: Abs[abs[expr_]] := abs[expr];
abs /: abs[numb_?NumberQ] := Abs[numb];
abs /: abs[(factor_?NumberQ) * expr_] := Abs[factor] * abs[expr];


replaceEnergy[particle_] := ReplaceAll[#,
  energy[particle]^\[Alpha]_ :> (
    (abs[momentum[particle]]^2 + mass[particle]^2)^(Quotient[\[Alpha], 2]) *
    energy[particle]^(Mod[\[Alpha], 2])
  )
]&;


replaceEnergy[particle_, All] := ReplaceAll[#,
  energy[particle]^\[Alpha]_. :> (abs[momentum[particle]]^2 + mass[particle]^2)^(\[Alpha]/2)
]&;


replaceMomentum[particle_] := ReplaceAll[#,
  abs[momentum[particle]]^\[Alpha]_. :> (
    (energy[particle]^2 - mass[particle]^2)^(Quotient[\[Alpha], 2]) *
    abs[momentum[particle]]^(Mod[\[Alpha], 2])
  )
]&;


replaceMomentum[particle_, All] := ReplaceAll[#,
  abs[momentum[particle]]^\[Alpha]_. :> (energy[particle]^2 - mass[particle]^2)^(\[Alpha]/2)
]&;


replaceMass[particle_] := ReplaceAll[#,
  mass[particle]^\[Alpha]_ :> (
    (energy[particle]^2 - abs[momentum[particle]]^2)^(Quotient[\[Alpha], 2]) *
    mass[particle]^(Mod[\[Alpha], 2])
  )
]&;


replaceMass[particle_, All] := ReplaceAll[#,
  mass[particle]^\[Alpha]_. :> (energy[particle]^2 - abs[momentum[particle]]^2)^(\[Alpha]/2)
]&;


setInvariants[
    ps:{p1_Symbol, __}, ms_List, pRules:{Rule[_Symbol, _]...}, spRules_List
  ] := Module[
  {
    ruleConservation,
    ruleMasses,
    psAll,
    eqs,
    spVars,
    spFixed,
    spDefinitions,
    spSolutions
  },
  spFixed = Cases[spRules, _sp];
  spDefinitions = DeleteCases[spRules, Alternatives@@spFixed];
  ruleConservation = Solve[Total[ps] == 0, p1] // Flatten // First;
  ruleMasses = Thread[Rule[Map[sp,ps], ms^2]];
  psAll = ps~Join~Map[First, pRules];
  eqs = With[{lhs = Flatten[Outer[sp, psAll, psAll]]},
      Thread[lhs == (lhs // ReplaceRepeated[#, pRules]& // ReplaceAll[ruleConservation])]
  ] // modify[_sp, Distribute] // ReplaceAll[ruleMasses] // 
    Union // DeleteCases[True] // modify[Equal[(-1)_sp, _], Map[-#&]];
  eqs = eqs~Join~(
    (Equal@@@spDefinitions) // ReplaceRepeated[#, pRules]& // ReplaceAll[ruleConservation] // modify[_sp, Distribute] // 
    ReplaceAll[ruleMasses]
  );
  spVars = Union@Cases[{eqs}, _sp, Infinity] // DeleteCases[#, Alternatives@@spFixed]&;
  spSolutions = Solve[eqs, spVars] // Flatten // Expand;
  Return[spSolutions~Join~ruleMasses // Union];
];


End[];


EndPackage[];
