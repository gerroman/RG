(* ::Package:: *)

(* ::Text:: *)
(*Definitions and functions to work with 4 vectors and invariants*)


BeginPackage["RG`Kinematics`", {
  "RG`Calculation`", (* modify *)
  "RG`Notation`" (* energy, mass, momentum, sp, ...*)
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
  setInvariants[{{l1, p1}, {l2, p2}}, {m1, M1, m2, m2}, {pRules}, {spRules}] set up all scalar products
  setInvariants[ps, {}, pRules, spRules] automatically evalate masses applying mass on ps
";


expandScalarProduct::usage = "
  expandScalarProduct[expr] replace scalar products using energies, momenta and spherical angles
";

getKinematicsCMS::usage = "
  getKinematicsCMS[{{p1, p2}, {p3, p4}}, {s, \[Theta]}] return kinematics \ 
rules for the center of mass frame
";
getMandelstam::usage = "
  getMandelstam[{{li, pi}, {lf, pf}}] return equations for mandelstam variables
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


setInvariants[ps_List, {}, pRules_, spRules_] :=
  setInvariants[ps, Map[mass, Flatten[ps]], pRules, spRules];

setInvariants[
    {pIn:{p1_, ___}, pOut:{___}},
    ms_List,
    pRules:{Rule[_, _]...},
    spRules:{(Rule[_sp, _] | (_sp))...}
  ] := Module[
  {
    ps = Join[pIn, pOut],
    ruleConservation,
    ruleMasses,
    psAll,
    eqs,
    spVars,
    spFixed,
    spDefinitions,
    spSolutions,
    result
  },
  
  spFixed = Cases[spRules, _sp];
  spDefinitions = DeleteCases[spRules, Alternatives@@spFixed];

  ruleConservation = p1 -> Total[Join[(-1) * Rest[pIn], pOut]];
  ruleMasses = Thread[Rule[Map[sp, ps], ms^2]];

  psAll = ps~Join~Map[First, pRules];
  
  eqs = With[
    {
      lhs = Flatten[Outer[sp, psAll, psAll]]
    },
    Thread@Equal[
      (
        (* do not use conservation law *)
        lhs // ReplaceAll[#, ruleMasses]& // ReplaceAll[#, spDefinitions]&
      ),
      (
        (* use conservation law & pRules*)
        lhs // ReplaceAll[#, pRules]& // modify[_sp, ReplaceAll[#, ruleConservation]&] //
	  modify[_sp, Distribute] // ReplaceAll[#, ruleMasses]& // ReplaceAll[#, spDefinitions]&
      )
    ] // Union// DeleteCases[True] (* drop m^2 == m^2 *)
  ]; 

  spVars = Union@Cases[eqs, _sp, Infinity] //
    DeleteCases[#, Alternatives@@spFixed]&;
  Assert[Length[spVars] == Length[eqs]];

  spSolutions = Solve[eqs, spVars] // Flatten;
  Assert[Length[spSolutions] == Length[spVars]];

  result = Sort[Join[spSolutions, ruleMasses, spDefinitions] // Expand];
  Return[result];
];

SetAttributes[theta, Orderless];
theta[x_, x_] = 0;

expandScalarProduct := ReplaceAll[{
  sp[a_, b_] :> (
    energy[a] * energy[b]
    - abs[momentum[a]] * abs[momentum[b]] * Cos[theta[momentum[a], momentum[b]]]
  )
}];

getKinematicsCMS[{{p1_, p2_}, {p3_, p4_}}, {s_, \[Theta]_}] := Module[
  {rule, var},
  rule = Association[{
    abs@momentum[p1] -> pcms,
    abs@momentum[p2] -> pcms,
    abs@momentum[p3] -> prime`pcms,
    abs@momentum[p4] -> prime`pcms
  }];

  Scan[
    AppendTo[
      rule,
      energy[#] // rewriteIt[replaceEnergy[#, All] /* ReplaceAll[rule]] // toRules
    ] &,
    {p1, p2, p3, p4}
  ];

  AppendTo[rule, theta[momentum[p1], momentum[p2]] -> \[Pi]];
  AppendTo[rule, theta[momentum[p1], momentum[p3]] -> \[Theta]];
  AppendTo[rule, theta[momentum[p1], momentum[p4]] -> \[Pi] - \[Theta]];
  AppendTo[rule, theta[momentum[p2], momentum[p3]] -> \[Pi] - \[Theta]];
  AppendTo[rule, theta[momentum[p2], momentum[p4]] -> \[Theta]];
  AppendTo[rule, theta[momentum[p3], momentum[p4]] -> \[Pi]];

  AppendTo[rule,
    pcms^2 -> ((s - (mass[p1] - mass[p2])^2)*(s - (mass[p1] + mass[p2])^2))/(4*s)
  ];

  AppendTo[rule,
    prime`pcms^2 -> ((s - (mass[p3] - mass[p4])^2)*(s - (mass[p3] + mass[p4])^2))/(4*s)
  ];
  
  Return[rule]
];


getMandelstam[{pIn:{li_, pi_}, pOut:{lf_, pf_}}, {s_, t_, u_}] := {
  s + t + u == Total[(mass[#]^2)& /@ (pIn~Join~pOut)],
  s == sp[li + pi],
  t == sp[li - lf],
  u == sp[li - pf]
};


End[];


EndPackage[];
