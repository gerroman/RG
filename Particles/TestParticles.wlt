Needs["RG`Particles`"]


test[1] := VerificationTest[(* #1 *)
	With[
		List[
			Set[keys, DeleteCases["All"][Keys[properties]]]
		],
		Complement[properties["All"], Union[Flatten[Map[properties, keys]]]]
	]
	,
	List[]	
]


test[2] := VerificationTest[(* #2 *)
	With[
		List[Set[values, Flatten[Map[properties][DeleteCases["All"][Keys[properties]]]]]],
		Equal[Length[Union[values]], Length[values]]
	]
	,
	True	
]


test[3] := 
VerificationTest[(* #3 *)
	With[{e = ParticleData["Electron"]}, {e, anti[e]} // symbolizeParticles]
	,
	{"e", OverBar["e"]}
]

test[4] :=
VerificationTest[(* #4 *)
  {particles[[21]]["Mass"], particles[[21]]["Symbol"]}
  ,
  {Quantity[139.57061`8., "Megaelectronvolts"/"SpeedOfLight"^2], Superscript["π", "+"]}
]

