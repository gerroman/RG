BeginTestSection["TestCalculation"]

VerificationTest[(* 1 *)
	CompoundExpression[Needs["RG`Calculation`"], Equal[UnderBar[f][][x], f[x]]]
	,
	True	
]

VerificationTest[(* 2 *)
	CompoundExpression[Needs["RG`Calculation`"], Equal[UnderBar[f][x][y], f[y, x]]]
	,
	True	
]

EndTestSection[]
