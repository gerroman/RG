(* ::Package:: *)

(* ::Text:: *)
(*Functions to perform calculations with explicit bispinors *)


BeginPackage["RG`HelicityStates`", {
  "RG`Notation`",
  "RG`Calculation`",
  "RG`Kinematics`",
  "RG`Traces`"
}];


eq`gamma\[LetterSpace]5::usage = "
  eq`gamma\[LetterSpace]5 --  definition of gamma[5]
";

\[Omega]::usage = "
  \[Omega][plus|minus]  -- projector to right(left) bispinors
";
eq`omega\[LetterSpace]plus\[LetterSpace]minus::usage = "
  eq`omega\[LetterSpace]plus\[LetterSpace]minus -- definition of projectors to right(left) bispinors 
";

eq`gamma\[LetterSpace]standard::usage = "
  eq`gamma\[LetterSpace]standard -- definiton of gamma matrix in spiral basis
";

rule`bispinor\[LetterSpace]explicit::usage = "
  rule\[LetterSpace]bispinor\[LetterSpace]explicit -- rule get explicit bispinor in spiral basis of gamma matrix
";

eq`z\[LetterSpace]kinematic::usage = "
  eq`z\[LetterSpace]kinematic -- definition of 4-momentum and bispinors in spiral basis
";

eq`theta\[LetterSpace]phi\[LetterSpace]kinematic::usage = "
  eq`theta\[LetterSpace]phi\[LetterSpace]kinematic[] -- definition of 4-momentum and bispinors in spiral basis
  eq`theta\[LetterSpace]phi\[LetterSpace]kinematic[id] use id as particle identificator
";

outer\[LetterSpace]dot::usage = "
  outer\[LetterSpace]dot[kinematic] -- evaluates products  u.bar`u and v.bar`v assuming specific kinematic
";


Begin["`Private`"];


eq`gamma\[LetterSpace]5 = (\[Gamma][5] == I Dot @@ Array[\[Gamma], 4, 0]);


setIndexed[\[Omega]];
eq`omega\[LetterSpace]plus\[LetterSpace]minus = {
  \[Omega][plus] == 1/2 * (id + \[Gamma][5])
  , \[Omega][minus] == 1/2 * (id - \[Gamma][5])
};


eq`gamma\[LetterSpace]standard = {
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

eq`gamma\[LetterSpace]standard = AppendTo[
  eq`gamma\[LetterSpace]standard,
  eq`gamma\[LetterSpace]5 // ReplaceAll[eq`gamma\[LetterSpace]standard // toRules]
];

eq`gamma\[LetterSpace]standard = Join[
   eq`gamma\[LetterSpace]standard,
	 eq`omega\[LetterSpace]plus\[LetterSpace]minus // ReplaceAll[eq`gamma\[LetterSpace]standard // toRules]
];


(* auxiliary functions: sigma, bar_sigma *)
sigma[p_List /; (Length[p] == 3)] := sp[Array[PauliMatrix, 3], p];
sigma[p_List /; (Length[p] == 4)] := sp[Prepend[Array[PauliMatrix, 3], IdentityMatrix[2]], p];
bar\[LetterSpace]sigma[p_List /; (Length[p] == 4)] := sp[Prepend[(-1) * Array[PauliMatrix, 3], IdentityMatrix[2]], p];

rule`bispinor\[LetterSpace]explicit = {
  u[p_List /; (Length[p] == 4), xi_List /; (Length[xi] == 2)] :> Flatten[{
    MatrixPower[sigma[p], 1/2].xi,
		MatrixPower[bar\[LetterSpace]sigma[p], 1/2].xi
  }],
  v[p_List /; (Length[p] == 4), eta_List /; (Length[eta] == 2)] :> Flatten[{
    MatrixPower[sigma[p], 1/2].eta,
		(-1) * MatrixPower[bar\[LetterSpace]sigma[p], 1/2].eta
  }],
  bar`u[p_List /; (Length[p] == 4), xi_List /; (Length[xi] == 2)] :> Conjugate[
    u[p, xi] // Replace[rule`bispinor\[LetterSpace]explicit]
  ].(\[Gamma][0] // Replace[eq`gamma\[LetterSpace]standard // toRules]),
	bar`v[p_List /; (Length[p] == 4), eta_List /; (Length[eta] == 2)] :> Conjugate[
    v[p, eta] // Replace[rule`bispinor\[LetterSpace]explicit]
  ].(\[Gamma][0] // Replace[eq`gamma\[LetterSpace]standard // toRules])
};


Block[{$Assumptions = \[ScriptCapitalE][\[ScriptP]] >= 0 && \[ScriptM] >= 0 && \[ScriptP] >= 0 && \[ScriptCapitalE][\[ScriptP]] >= 0 && \[ScriptCapitalE][\[ScriptP]] >= \[ScriptP]},
  eq`z\[LetterSpace]kinematic = {
    \[ScriptCapitalE][\[ScriptP]] == Sqrt[\[ScriptP]^2 + \[ScriptM]^2],
    \[GothicP]["z"] == {\[ScriptCapitalE][\[ScriptP]], 0, 0, \[ScriptP]}
  };

  eq`z\[LetterSpace]kinematic = eq`z\[LetterSpace]kinematic~Join~(
   {
      \[Xi]["z", "+"] == {1, 0},
      \[Xi]["z", "-"] == {0, 1},
      \[Eta]["z", "+"] == {0, 1},
      \[Eta]["z", "-"] == {-1, 0}
    }
  );

  eq`z\[LetterSpace]kinematic = eq`z\[LetterSpace]kinematic~Join~(
	  {
      u["z", "+"] == u[\[GothicP]["z"], \[Xi]["z", "+"]]
      , u["z", "-"] == u[\[GothicP]["z"], \[Xi]["z", "-"]]
      , v["z", "+"] == v[\[GothicP]["z"], \[Eta]["z", "+"]]
      , v["z", "-"] == v[\[GothicP]["z"], \[Eta]["z", "-"]]
    } // rewriteIt[
	    ReplaceAll[eq`z\[LetterSpace]kinematic // toRules] /* Replace[rule`bispinor\[LetterSpace]explicit]
    ]
	);

  eq`z\[LetterSpace]kinematic = eq`z\[LetterSpace]kinematic~Join~(
    {
      bar`u["z", "+"] == bar`u[\[GothicP]["z"], \[Xi]["z", "+"]]
      , bar`u["z", "-"] == bar`u[\[GothicP]["z"], \[Xi]["z", "-"]]
      , bar`v["z", "+"] == bar`v[\[GothicP]["z"], \[Eta]["z", "+"]]
      , bar`v["z", "-"] == bar`v[\[GothicP]["z"], \[Eta]["z", "-"]]
    } // rewriteIt[
      ReplaceAll[eq`z\[LetterSpace]kinematic // toRules] /* Replace[rule`bispinor\[LetterSpace]explicit] /* Simplify
    ]
  );

  eq`z\[LetterSpace]kinematic = eq`z\[LetterSpace]kinematic // Reverse;
];


Block[{$Assumptions = (
    (0 <= \[Theta] <= \[Pi]) && (-\[Pi] < \[CurlyPhi] <= \[Pi]) &&
    (\[ScriptCapitalE][\[ScriptP]] >= 0) && (\[ScriptM] >= 0) && (\[ScriptP] >= 0) &&
    (\[ScriptCapitalE][\[ScriptP]] >= \[ScriptP]) && (\[ScriptCapitalE][\[ScriptP]] >= \[ScriptM])
  )},

  eq`theta\[LetterSpace]phi\[LetterSpace]kinematic[] = {
      \[ScriptCapitalE][\[ScriptP]] == Sqrt[\[ScriptP]^2 + \[ScriptM]^2],
      \[GothicP][{\[Theta], \[CurlyPhi]}] == {\[ScriptCapitalE][\[ScriptP]], \[ScriptP] Sin[\[Theta]] Cos[\[CurlyPhi]], \[ScriptP] Sin[\[Theta]] Sin[\[CurlyPhi]], \[ScriptP] Cos[\[Theta]]}
  };

  eq`theta\[LetterSpace]phi\[LetterSpace]kinematic[] = eq`theta\[LetterSpace]phi\[LetterSpace]kinematic[]~Join~(
     {
        \[Xi][{\[Theta], \[CurlyPhi]}, "+"] == {Cos[\[Theta]/2], 
        Exp[I \[CurlyPhi]] Sin[\[Theta]/2]},
        \[Xi][{\[Theta], \[CurlyPhi]}, 
        "-"] == {-Exp[-I \[CurlyPhi]] Sin[\[Theta]/2], Cos[\[Theta]/2]}
      });

  eq`theta\[LetterSpace]phi\[LetterSpace]kinematic[] = eq`theta\[LetterSpace]phi\[LetterSpace]kinematic[]~Join~({
    \[Eta][{\[Theta], \[CurlyPhi]}, "+"] == -I PauliMatrix[2].Conjugate[\[Xi][{\[Theta], \[CurlyPhi]}, "+"]],
    \[Eta][{\[Theta], \[CurlyPhi]}, "-"] == -I PauliMatrix[2].Conjugate[\[Xi][{\[Theta], \[CurlyPhi]}, "-"]]
  } // rewriteIt[
    ReplaceAll[eq`theta\[LetterSpace]phi\[LetterSpace]kinematic[] // toRules] /* 
    ComplexExpand /* ExpToTrig /* Factor /* 
    groupIt[{Exp[I \[CurlyPhi]], Exp[-I \[CurlyPhi]]}, ExpToTrig]
  ]);

  eq`theta\[LetterSpace]phi\[LetterSpace]kinematic[] = eq`theta\[LetterSpace]phi\[LetterSpace]kinematic[]~Join~({
    u[{\[Theta], \[CurlyPhi]}, "+"] == u[\[GothicP][{\[Theta], \[CurlyPhi]}], \[Xi][{\[Theta], \[CurlyPhi]}, "+"]], 
    u[{\[Theta], \[CurlyPhi]}, "-"] == u[\[GothicP][{\[Theta], \[CurlyPhi]}], \[Xi][{\[Theta], \[CurlyPhi]}, "-"]], 
    v[{\[Theta], \[CurlyPhi]}, "+"] == v[\[GothicP][{\[Theta], \[CurlyPhi]}], \[Eta][{\[Theta], \[CurlyPhi]}, "+"]], 
    v[{\[Theta], \[CurlyPhi]}, "-"] == v[\[GothicP][{\[Theta], \[CurlyPhi]}], \[Eta][{\[Theta], \[CurlyPhi]}, "-"]]
  } // rewriteIt[
    ReplaceAll[eq`theta\[LetterSpace]phi\[LetterSpace]kinematic[] // toRules] /* Replace[rule`bispinor\[LetterSpace]explicit] /* Simplify
  ]);

  eq`theta\[LetterSpace]phi\[LetterSpace]kinematic[] = eq`theta\[LetterSpace]phi\[LetterSpace]kinematic[]~Join~({
    bar`u[{\[Theta], \[CurlyPhi]}, "+"] == bar`u[\[GothicP][{\[Theta], \[CurlyPhi]}], \[Xi][{\[Theta], \[CurlyPhi]}, "+"]]
    , bar`u[{\[Theta], \[CurlyPhi]}, "-"] == bar`u[\[GothicP][{\[Theta], \[CurlyPhi]}], \[Xi][{\[Theta], \[CurlyPhi]}, "-"]]
    , bar`v[{\[Theta], \[CurlyPhi]}, "+"] == bar`v[\[GothicP][{\[Theta], \[CurlyPhi]}], \[Eta][{\[Theta], \[CurlyPhi]}, "+"]]
    , bar`v[{\[Theta], \[CurlyPhi]}, "-"] == bar`v[\[GothicP][{\[Theta], \[CurlyPhi]}], \[Eta][{\[Theta], \[CurlyPhi]}, "-"]]
  } // rewriteIt[
    ReplaceAll[eq`theta\[LetterSpace]phi\[LetterSpace]kinematic[] // toRules] /* Replace[rule`bispinor\[LetterSpace]explicit] /* Simplify
  ]);

  eq`theta\[LetterSpace]phi\[LetterSpace]kinematic[] = eq`theta\[LetterSpace]phi\[LetterSpace]kinematic[] // Reverse;
];

eq`theta\[LetterSpace]phi\[LetterSpace]kinematic[id_] := eq`theta\[LetterSpace]phi\[LetterSpace]kinematic[] // ReplaceAll[{
  {\[Theta], \[CurlyPhi]} -> id
  , \[Theta] -> \[Theta][id]
  , \[CurlyPhi] -> \[CurlyPhi][id]
  , \[ScriptP] -> \[ScriptP][id]
  , \[ScriptM] -> \[ScriptM][id]
}];


outer\[LetterSpace]dot[kinematic_][expr_] := (expr // ReplaceAll[
  (Dot[a : (_u), b : (_bar`u)] | Dot[a : (_v), b : (_bar`v)]) :> Outer[
    Times,
		a // ReplaceAll[kinematic // toRules],
		b // ReplaceAll[kinematic // toRules]
  ]
]);


End[];


EndPackage[];
