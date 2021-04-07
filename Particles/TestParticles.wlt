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
];
