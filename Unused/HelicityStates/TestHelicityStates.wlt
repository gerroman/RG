Needs["RG`HelicityStates`"];


test[1] := VerificationTest[(* #1 *)
	eq`gamma\[LetterSpace]5 // ReplaceAll[eq`gamma\[LetterSpace]standard // toRules]
	,
	True
];


metricTensor = DiagonalMatrix[{1, -1, -1, -1}];


test[2] := VerificationTest[(* #2 *)
  Table[
	  (\[Gamma][i].\[Gamma][j] + \[Gamma][j].\[Gamma][i] - 2 * metricTensor[[i + 1, j + 1]] * id == zero //
		  ReplaceAll[eq`gamma\[LetterSpace]standard // toRules]
		)
	  , {i, 0, 3}, {j, 0, 3}
	] // Flatten // Apply[And]
	,
	True
]


test[3] := VerificationTest[(* #3 *)
  Table[
	  (\[Gamma][i].\[Gamma][5] + \[Gamma][5].\[Gamma][i] == zero //
		  ReplaceAll[eq`gamma\[LetterSpace]standard // toRules]
		)
	  , {i, 0, 3}
	] // Flatten // Apply[And]
	,
	True
]


test[4] := VerificationTest[(* #4 *)
  (outer\[LetterSpace]dot[eq`z\[LetterSpace]kinematic][u["z", "+"].bar`u["z", "+"] + u["z", "-"].bar`u["z", "-"]] ==
	  (\[Gamma][\[GothicP]["z"] // ReplaceAll[eq`z\[LetterSpace]kinematic // toRules]]) + m * id) // ReplaceAll[eq`gamma\[LetterSpace]standard // toRules] //
		ReplaceAll[m -> Sqrt[\[ScriptCapitalE][\[ScriptP]] + \[ScriptP]] Sqrt[\[ScriptCapitalE][\[ScriptP]] - \[ScriptP]]] // Simplify
	,
	True
]


test[5] := VerificationTest[(* #5 *)
  (outer\[LetterSpace]dot[eq`z\[LetterSpace]kinematic][v["z", "+"].bar`v["z", "+"] + v["z", "-"].bar`v["z", "-"]] ==
	  (\[Gamma][\[GothicP]["z"] // ReplaceAll[eq`z\[LetterSpace]kinematic // toRules]]) - m * id) // ReplaceAll[eq`gamma\[LetterSpace]standard // toRules] //
		ReplaceAll[m -> Sqrt[\[ScriptCapitalE][\[ScriptP]] + \[ScriptP]] Sqrt[\[ScriptCapitalE][\[ScriptP]] - \[ScriptP]]] // Simplify
	,
	True
]


# TODO: more tests
