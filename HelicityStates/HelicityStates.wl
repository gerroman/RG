(* ::Package:: *)

(* ::Text:: *)
(*Functions to perform calculations with explicit bispinors *)


BeginPackage["RG`HelicityStates`", {
  "RG`CommonNotation`",
  "RG`Notation`",
  "RG`Calculation`",
  "RG`Kinematics`",
  "RG`Traces`"
}];


eq`gamma5::usage = "
  eq`gamma5 \[LongDash]  definition of gamma[5]
";

\[Omega]::usage = "
  \[Omega][plus|minus]  \[LongDash] projector to right(left) bispinors
";
eq`omegaplusminus::usage = "
  eq`omegaplusminus \[LongDash] definition of projectors to right(left) bispinors
";

eq`gammastandard::usage = "
  eq`gammastandard \[LongDash] definiton of gamma matrix in spiral basis
";

rule`bispinorexplicit::usage = "
  rulebispinorexplicit \[LongDash] rule get explicit bispinor in spiral basis of gamma matrix
";

eq`zkinematic::usage = "
  eq`zkinematic \[LongDash] definition of 4-momentum and bispinors in spiral basis
";

eq`thetaphikinematic::usage = "
  eq`thetaphikinematic[] \[LongDash] definition of 4-momentum and bispinors in spiral basis
  eq`thetaphikinematic[id] use id as particle identificator
";

eq`zkinematicmassless::usage = "
  eq`zkinematicmassless \[LongDash] definition of 4-momentum and bispinors in spiral basis
";

eq`thetaphikinematicmassless::usage = "
  eq`thetaphikinematicmassless[] \[LongDash] definition of 4-momentum and bispinors in spiral basis
  eq`thetaphikinematicmassless[id] use id as particle identificator
";

outerdot::usage = "
  outerdot[kinematic] \[LongDash] evaluates products  u.bar`u and v.bar`v assuming specific kinematic
";


SProduct::usage = "
  SProduct[\[GothicP][i_], \[GothicP][j_]] \[LongDash] base product for expressions with massless bispinor
";

rule`SProduct::usage "
  rule`SProduct \[LongDash] rule base product explicit representation
";


Begin["`Private`"];


eq`gamma5 = (\[Gamma][5] == I Dot @@ Array[\[Gamma], 4, 0]);


setIndexed[\[Omega]];
eq`omegaplusminus = {
  \[Omega][plus] == 1/2 * (id + \[Gamma][5])
  , \[Omega][minus] == 1/2 * (id - \[Gamma][5])
};


eq`gammastandard = {
  zero == ConstantArray[0, {4, 4}],
  id == IdentityMatrix[4],
  \[Gamma][0] == ArrayFlatten[{
    {ConstantArray[0, {2, 2}], IdentityMatrix[2]},
    {IdentityMatrix[2], ConstantArray[0, {2, 2}]}
  }]
}~Join~Array[
  (\[Gamma][#] == ArrayFlatten@{
    {ConstantArray[0, {2, 2}], PauliMatrix[#]},
		{-PauliMatrix[#], ConstantArray[0, {2, 2}]}
  })&, 3
];

eq`gammastandard = AppendTo[
  eq`gammastandard,
  eq`gamma5 // ReplaceAll[eq`gammastandard // toRules]
];

eq`gammastandard = Join[
   eq`gammastandard,
	 eq`omegaplusminus // ReplaceAll[eq`gammastandard // toRules]
];


(* auxiliary functions: sigma, bar_sigma *)
sigma[p_List /; (Length[p] == 3)] := sp[Array[PauliMatrix, 3], p];
sigma[p_List /; (Length[p] == 4)] := sp[Prepend[Array[PauliMatrix, 3], IdentityMatrix[2]], p];
barsigma[p_List /; (Length[p] == 4)] := sp[Prepend[(-1) * Array[PauliMatrix, 3], IdentityMatrix[2]], p];

rule`bispinorexplicit = {
  u[p_List /; (Length[p] == 4), xi_List /; (Length[xi] == 2)] :> Flatten[{
    MatrixPower[sigma[p], 1/2].xi,
		MatrixPower[barsigma[p], 1/2].xi
  }],
  v[p_List /; (Length[p] == 4), eta_List /; (Length[eta] == 2)] :> Flatten[{
    MatrixPower[sigma[p], 1/2].eta,
		(-1) * MatrixPower[barsigma[p], 1/2].eta
  }],
  bar`u[p_List /; (Length[p] == 4), xi_List /; (Length[xi] == 2)] :> Conjugate[
    u[p, xi] // Replace[rule`bispinorexplicit]
  ].(\[Gamma][0] // Replace[eq`gammastandard // toRules]),
	bar`v[p_List /; (Length[p] == 4), eta_List /; (Length[eta] == 2)] :> Conjugate[
    v[p, eta] // Replace[rule`bispinorexplicit]
  ].(\[Gamma][0] // Replace[eq`gammastandard // toRules])
};


Block[{$Assumptions = \[ScriptCapitalE][\[ScriptP]] >= 0 && \[ScriptM] >= 0 && \[ScriptP] >= 0 && \[ScriptCapitalE][\[ScriptP]] >= 0 && \[ScriptCapitalE][\[ScriptP]] >= \[ScriptP]},
  eq`zkinematic = {
    \[ScriptCapitalE][\[ScriptP]] == Sqrt[\[ScriptP]^2 + \[ScriptM]^2],
    \[GothicP]["z"] == {\[ScriptCapitalE][\[ScriptP]], 0, 0, \[ScriptP]}
  };

  eq`zkinematic = eq`zkinematic~Join~(
   {
      \[Xi]["z", plus] == {1, 0},
      \[Xi]["z", minus] == {0, 1},
      \[Eta]["z", plus] == {0, 1},
      \[Eta]["z", minus] == {-1, 0}
    }
  );

  eq`zkinematic = eq`zkinematic~Join~(
	  {
      u["z", plus] == u[\[GothicP]["z"], \[Xi]["z", plus]]
      , u["z", minus] == u[\[GothicP]["z"], \[Xi]["z", minus]]
      , v["z", plus] == v[\[GothicP]["z"], \[Eta]["z", plus]]
      , v["z", minus] == v[\[GothicP]["z"], \[Eta]["z", minus]]
    } // rewriteIt[
	    ReplaceAll[eq`zkinematic // toRules] /* Replace[rule`bispinorexplicit]
    ]
	);

  eq`zkinematic = eq`zkinematic~Join~(
    {
      bar`u["z", plus] == bar`u[\[GothicP]["z"], \[Xi]["z", plus]]
      , bar`u["z", minus] == bar`u[\[GothicP]["z"], \[Xi]["z", minus]]
      , bar`v["z", plus] == bar`v[\[GothicP]["z"], \[Eta]["z", plus]]
      , bar`v["z", minus] == bar`v[\[GothicP]["z"], \[Eta]["z", minus]]
    } // rewriteIt[
      ReplaceAll[eq`zkinematic // toRules] /* Replace[rule`bispinorexplicit] /* Simplify
    ]
  );

  eq`zkinematic = eq`zkinematic // Reverse;
];

thetaPhiAssumtions = (
    (0 <= \[Theta] <= \[Pi]) && (-\[Pi] < \[CurlyPhi] <= \[Pi]) &&
    (\[ScriptCapitalE][\[ScriptP]] >= 0) && (\[ScriptM] >= 0) && (\[ScriptP] >= 0) &&
    (\[ScriptCapitalE][\[ScriptP]] >= \[ScriptP]) && (\[ScriptCapitalE][\[ScriptP]] >= \[ScriptM])
  );
Block[{$Assumptions = thetaPhiAssumtions},

  eq`thetaphikinematic[] = {
      \[ScriptCapitalE][\[ScriptP]] == Sqrt[\[ScriptP]^2 + \[ScriptM]^2],
      \[GothicP][{\[Theta], \[CurlyPhi]}] == {\[ScriptCapitalE][\[ScriptP]], \[ScriptP] Sin[\[Theta]] Cos[\[CurlyPhi]], \[ScriptP] Sin[\[Theta]] Sin[\[CurlyPhi]], \[ScriptP] Cos[\[Theta]]}
  };

  eq`thetaphikinematic[] = eq`thetaphikinematic[]~Join~(
     {
        \[Xi][{\[Theta], \[CurlyPhi]}, plus] == {Cos[\[Theta]/2],
        Exp[I \[CurlyPhi]] Sin[\[Theta]/2]},
        \[Xi][{\[Theta], \[CurlyPhi]},
        minus] == {-Exp[-I \[CurlyPhi]] Sin[\[Theta]/2], Cos[\[Theta]/2]}
      });

  eq`thetaphikinematic[] = eq`thetaphikinematic[]~Join~({
    \[Eta][{\[Theta], \[CurlyPhi]}, plus] == -I PauliMatrix[2].Conjugate[\[Xi][{\[Theta], \[CurlyPhi]}, plus]],
    \[Eta][{\[Theta], \[CurlyPhi]}, minus] == -I PauliMatrix[2].Conjugate[\[Xi][{\[Theta], \[CurlyPhi]}, minus]]
  } // rewriteIt[
    ReplaceAll[eq`thetaphikinematic[] // toRules] /*
    ComplexExpand /* ExpToTrig /* Factor /*
    groupIt[{Exp[I \[CurlyPhi]], Exp[-I \[CurlyPhi]]}, ExpToTrig]
  ]);

  eq`thetaphikinematic[] = eq`thetaphikinematic[]~Join~({
    u[{\[Theta], \[CurlyPhi]}, plus] == u[\[GothicP][{\[Theta], \[CurlyPhi]}], \[Xi][{\[Theta], \[CurlyPhi]}, plus]],
    u[{\[Theta], \[CurlyPhi]}, minus] == u[\[GothicP][{\[Theta], \[CurlyPhi]}], \[Xi][{\[Theta], \[CurlyPhi]}, minus]],
    v[{\[Theta], \[CurlyPhi]}, plus] == v[\[GothicP][{\[Theta], \[CurlyPhi]}], \[Eta][{\[Theta], \[CurlyPhi]}, plus]],
    v[{\[Theta], \[CurlyPhi]}, minus] == v[\[GothicP][{\[Theta], \[CurlyPhi]}], \[Eta][{\[Theta], \[CurlyPhi]}, minus]]
  } // rewriteIt[
    ReplaceAll[eq`thetaphikinematic[] // toRules] /* Replace[rule`bispinorexplicit] /* Simplify
  ]);

  eq`thetaphikinematic[] = eq`thetaphikinematic[]~Join~({
    bar`u[{\[Theta], \[CurlyPhi]}, plus] == bar`u[\[GothicP][{\[Theta], \[CurlyPhi]}], \[Xi][{\[Theta], \[CurlyPhi]}, plus]]
    , bar`u[{\[Theta], \[CurlyPhi]}, minus] == bar`u[\[GothicP][{\[Theta], \[CurlyPhi]}], \[Xi][{\[Theta], \[CurlyPhi]}, minus]]
    , bar`v[{\[Theta], \[CurlyPhi]}, plus] == bar`v[\[GothicP][{\[Theta], \[CurlyPhi]}], \[Eta][{\[Theta], \[CurlyPhi]}, plus]]
    , bar`v[{\[Theta], \[CurlyPhi]}, minus] == bar`v[\[GothicP][{\[Theta], \[CurlyPhi]}], \[Eta][{\[Theta], \[CurlyPhi]}, minus]]
  } // rewriteIt[
    ReplaceAll[eq`thetaphikinematic[] // toRules] /* Replace[rule`bispinorexplicit] /* Simplify
  ]);

  eq`thetaphikinematic[] = eq`thetaphikinematic[] // Reverse;
];

eq`thetaphikinematic[id_] := eq`thetaphikinematic[] // ReplaceAll[{
  {\[Theta], \[CurlyPhi]} -> id,
	\[Theta] -> \[Theta][id],
	\[CurlyPhi] -> \[CurlyPhi][id],
	\[ScriptP] -> \[ScriptP][id],
	\[ScriptM] -> \[ScriptM][id]
}];


outerdot[kinematic_][expr_] := (expr // ReplaceAll[
  (Dot[a : (_u), b : (_bar`u)] | Dot[a : (_v), b : (_bar`v)]) :> Outer[
    Times,
		a // ReplaceAll[kinematic // toRules],
		b // ReplaceAll[kinematic // toRules]
  ]
]);


eq`zkinematicmassless = eq`zkinematic // ReplaceRepeated[#, {\[ScriptCapitalE][\[ScriptP]] -> \[ScriptP], \[ScriptM] -> 0, Sqrt[\[ScriptP]^2] -> \[ScriptP]}]& // Cases[_Equal];
eq`thetaphikinematicmassless[] = eq`thetaphikinematic[] // ReplaceRepeated[#, {\[ScriptCapitalE][\[ScriptP]] -> \[ScriptP], \[ScriptM] -> 0, Sqrt[\[ScriptP]^2] -> \[ScriptP]}]& // Cases[_Equal];
eq`thetaphikinematicmassless[id_] := eq`thetaphikinematicmassless[] // ReplaceAll[{
  {\[Theta], \[CurlyPhi]} -> id,
	\[Theta] -> \[Theta][id],
	\[CurlyPhi] -> \[CurlyPhi][id],
	\[ScriptP] -> \[ScriptP][id]
}];


SProduct /: Format[SProduct[a_, b_], TraditionalForm] := HoldForm[Global`S][a, b];
rule`SProduct = {
  SProduct[\[GothicP][i_], \[GothicP][j_]] :> 2 * Sqrt[\[ScriptP][i]] * Sqrt[\[ScriptP][j]] * (
	  Exp[-I * \[CurlyPhi][i]] * Sin[\[Theta][i]/2] * Cos[\[Theta][j]/2]
		- Exp[-I * \[CurlyPhi][j]] * Cos[\[Theta][i]/2] * Sin[\[Theta][j]/2]
  ),
	Conjugate[SProduct[\[GothicP][i_], \[GothicP][j_]]] :> 2 * Sqrt[\[ScriptP][i]] * Sqrt[\[ScriptP][j]] * (
	  Exp[I * \[CurlyPhi][i]] * Sin[\[Theta][i]/2] * Cos[\[Theta][j]/2]
		- Exp[I * \[CurlyPhi][j]] * Cos[\[Theta][i]/2] * Sin[\[Theta][j]/2]
  )
} // Reverse;


End[];

Echo[$Context];

EndPackage[];
