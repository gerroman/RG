BeginTestSection["TestBaseUtils"]

VerificationTest[(* 1 *)
	CompoundExpression[Needs["RG`BaseUtils`"], RG`BaseUtils`assert[Greater[1, 2]]]
	,
	Null
	,
	{Assert::asrtfl}
]

VerificationTest[(* 2 *)
	CompoundExpression[Needs["RG`BaseUtils`"], RG`BaseUtils`assert[Greater[2, 1]]]
	,
	Null	
]

EndTestSection[]
