BeginTestSection["TestBaseUtils"]

Needs["RG`BaseUtils`"];

VerificationTest[(* 1 *)
	assert[Greater[1, 2]]
	,
	Null
	,
	{Assert::asrtfl}
]

VerificationTest[(* 2 *)
	assert[Greater[2, 1]]
	,
	Null	
]

VerificationTest[(* 3 *)
	modify[x, f][x]
	,
	f[x]	
]

VerificationTest[(* 4 *)
	modify[x, f, g][x]
	,
	g[f[x]]	
]

VerificationTest[(* 5 *)
	modify[Blank[Symbol], f][List[x, y]]
	,
	List[f[x], f[y]]	
]

VerificationTest[(* 6 *)
	modify[List[x], f][List[x, y]]
	,
	List[f[x], y]	
]

VerificationTest[(* 7 *)
	modify[List[Expand[Power[Plus[c, d], 2]]], RightComposition[Factor, ReplaceAll[Rule[c, 1]]]][Plus[Power[a, 2], Times[2, a, b], Power[b, 2], Power[c, 2], Times[2, c, d], Power[d, 2]]]
	,
	Plus[Power[a, 2], Times[2, a, b], Power[b, 2], Power[Plus[1, d], 2]]	
]

EndTestSection[]
