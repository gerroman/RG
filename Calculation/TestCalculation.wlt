BeginTestSection["TestCalculation"]


Needs["RG`Calculation`"];


VerificationTest[(* 1 *)
	Equal[OverTilde[f][][x], f[x]]
	,
	True	
]

VerificationTest[(* 2 *)
	Equal[OverTilde[f][x][y], f[y, x]]
	,
	True	
]

EndTestSection[]
