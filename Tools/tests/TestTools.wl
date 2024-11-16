Needs["RG`Tools`"]


VerificationTest[
	x // hold[x]
	,
	Hold[x]
]


VerificationTest[
	{Hold[x], Hold[y]} // release[y]
	,
	{Hold[x], y}
]


VerificationTest[
	Abs[Sqrt[x^2z]y] // powerExpand[x|z]
	,
	x Sqrt[z] Abs[y]
]


VerificationTest[
	x a + x a b + c // factorIt[x] // factorIt[a]
	,
	x a (1 + b) + c
]


VerificationTest[
	x a + x a b + c // pullIt[x] // pullIt[a]
	,
	x a (1 + b + c / (x a))
]


VerificationTest[
	x a + x a b + c // groupIt[x a (1+b)]
	,
	x a (1 + b) + c
]


VerificationTest[
	sp[a, b c] // pullFactor[b, sp]
	,
	b sp[a, c]
]


VerificationTest[
	sp[c, a - b] // distribute[sp] // pullFactor[_Integer, sp]
	,
	sp[c, a] - sp[c, b]
]


VerificationTest[
	c/(a-b) // changeSign[a-b]
	,
	(-c)/(b-a)
]


VerificationTest[
	eq[Sin[30 Degree]]
	,
	HoldForm[Sin[30 Degree]]==1/2
]


VerificationTest[
	a + x b + x^2 Sin[x] // cases[_Symbol]
	,
	{a, b, x}
]


VerificationTest[
	a + x b + x^2 Sin[x] // modify[x _ + x^2 _ , Factor]
	,
	a + x (b + x Sin[x])
]