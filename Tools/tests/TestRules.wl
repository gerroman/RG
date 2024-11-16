Get["RG/Tools/Rules.wl"]

VerificationTest[
	x //. rule`hold[{x}]
	,
	Hold[x]
]


VerificationTest[
	{Hold[x], Hold[y]} //. rule`release[y]
	,
	{Hold[x], y}
]


VerificationTest[
	Abs[Sqrt[x^2z]y] //. rule`powerExpand[x|z]
	,
	x Sqrt[z] Abs[y]
]


VerificationTest[
	x a + x a b + c //.rule`factor[x] //. rule`factor[a]
	,
	x a (1 + b) + c
]


VerificationTest[
	x a + x a b + c //. rule`pull[x] //. rule`pull[a]
	,
	x a (1 + b + c / (x a))
]


VerificationTest[
	x a + x a b + c /. rule`group[x a (1+b)]
	,
	x a (1 + b) + c
]


VerificationTest[
	sp[a, b c] /. rule`pullFactor[b, sp]
	,
	b sp[a, c]
]

VerificationTest[
	sp[c, a + b] /. rule`distribute[sp, Plus]
	,
	sp[c, a] + sp[c, b]
]