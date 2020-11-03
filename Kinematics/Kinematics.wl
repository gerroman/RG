(* ::Package:: *)
(* Definitions and functions to work with 4 vectors and invariants *)


BeginPackage["RG`Kinematics`", {"RG`Calculation`", "RG`Notation`"}]


energy::usage = "
  energy[p] represent time part of 4-vector
";

momentum::usage = "
  momentum[p] represent spatial part of 4-vector 
";

mass::usage = "
  mass[p] represent mass of 4-vector (i.e. energy[p]^2 - abs[momentum[p]]^2 == mass[p]^2) of 4-vector
";

abs::usage = "
  abs[p] represent module of spatial vector p
";

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


Begin["`Private`"]


protect[Global`m];
setIndexed[mass];
mass /: Format[mass, TraditionalForm] = Global`m;


energy /: Format[energy[expr_], TraditionalForm] := Superscript[expr, 0];
energy[expr_Plus] := energy /@ expr;
energy[expr_ * factor_?NumberQ] := factor * energy[expr];


momentum /: Format[momentum[expr_], TraditionalForm] := Style[expr, Bold, Italic];
momentum[expr_Plus] := momentum /@ expr;
momentum[expr_ * factor_?NumberQ] := factor * momentum[expr];


abs /: Format[abs[expr_], TraditionalForm] := BracketingBar[expr];
abs /: Abs[abs[expr_]] := abs[expr];
abs /: abs[numb_?NumberQ] := Abs[numb];
abs /: abs[(numb_?NumberQ) * expr_] := Abs[n] * abs[expr];


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
  abs[momentum[particle]]^\[Alpha]_ :> (
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



End[]


EndPackage[]