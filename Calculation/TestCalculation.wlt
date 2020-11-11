BeginTestSection["TestCalculation"]

Needs["RG`Calculation`"];

VerificationTest[
	modify[x, f][x]
	,
	f[x]	
]

VerificationTest[
	modify[x, f /* g][x]
	,
	g[f[x]]	
]

VerificationTest[
	modify[Blank[Symbol], f][List[x, y]]
	,
	List[f[x], f[y]]	
]

VerificationTest[
	modify[List[x], f][List[x, y]]
	,
	List[f[x], y]	
]

VerificationTest[
	modify[List[Expand[Power[Plus[c, d], 2]]], RightComposition[Factor, ReplaceAll[Rule[c, 1]]]][Plus[Power[a, 2], Times[2, a, b], Power[b, 2], Power[c, 2], Times[2, c, d], Power[d, 2]]]
	,
	Plus[Power[a, 2], Times[2, a, b], Power[b, 2], Power[Plus[1, d], 2]]	
]

VerificationTest[
  f[1, 2 a, 3 b] // pullFactors[{2}, f]
  ,
  2 f[1, a, 3 b]
]

VerificationTest[
  f[1, 2 a, 3 b] // pullFactors[{2, 3}, f]
  ,
  6 f[1, a, b]
]

VerificationTest[
  f[1, 2 a, 3 b] // pullFactors[_Integer, f]
  ,
  6 f[1, a, b]
]

VerificationTest[
  a // fixedPoint[# + 1&, 3]
  ,
  a + 3
]

VerificationTest[
  Hold[HoldForm[Hold[a]]] // release
  ,
  a
]

VerificationTest[
  a^2 + b^2 + 2 a b + c^2 + 5 // groupIt[(a+b)^2]
  ,
  (a + b)^2 + c^2 + 5
]

VerificationTest[
  1 + a x + 5 x + b x+ x^2 // factorIt[x]
  ,
  1 + x (a + 5 + b) + x^2
]

VerificationTest[
  1 + a x + 5 x + x^2 // pullIt[x]
  ,
  x^2 (a/x + 5/x + 1/x^2 + 1)
]

VerificationTest[
  1 + a x + 5 x + x^2 // pullIt[x, Identity, Plus, 1]
  ,
  x (a + 5 + 1/x + x)
]
VerificationTest[
  1 + a x + 5 x + b x+ x^2 // factorIt[{x}]
  ,
  1 + x (a + 5 + b) + x^2
]

VerificationTest[
  1 + a x + 5 x + x^2 // pullIt[{x}]
  ,
  x^2 (a/x + 5/x + 1/x^2 + 1)
]

VerificationTest[
  1 + a x + 5 x + x^2 // pullIt[{x}, Identity, Plus, 1]
  ,
  x (a + 5 + 1/x + x)
]

VerificationTest[
  powersPattern[{}]
  ,
  {1}
]
VerificationTest[
  powersPattern[{a, b}]
  ,
  {a^(_.)*b^(_.), b^(_.), a^(_.), 1}
]

VerificationTest[
  a * b + 3 * a + 3 * c * a * b + 3 * b + 1 //
  {
    Collect[#, Most[powersPattern[{a, b}]]]&,
    Collect[#, {a, b}]&
  } // Through
  ,
  {
    a * b (1 + 3 c) + 3 * a + 3 * b + 1,
    1 + 3*b + a*(3 + b*(1 + 3*c))
  }
]


VerificationTest[
  rewriteIt[
    f == a^2 + b^2 + 2 a b,
    groupIt[(a + b)^2]
  ]
  ,
  f == (a + b)^2
]

VerificationTest[
  b (a - b) // changeSign[a - b]
  ,
  - b (b - a)
]

VerificationTest[
  (a - b) // changeSign[a - b]
  ,
  a - b
]

VerificationTest[
  a == b // rewriteIt[ReplaceAll[b -> c]]
  ,
  a == c
]

VerificationTest[
  a == b * a // rewriteIt@@ReplaceAll/@{a -> d, b -> c}
  ,
  d == c * a
]

VerificationTest[
  a // rewriteIt[ReplaceAll[a -> b]]
  ,
  a == b
]

VerificationTest[
  Sqrt[a^2] + Log[b^2] // powerExpand
  ,
  a + Log[b^2]
]

VerificationTest[
  Log[a] + Log[b] // collectLogs
  ,
  Log[a b]
]

VerificationTest[
  2 c Log[a] - 2 c Log[b] // collectLogs
  ,
  2 c Log[a/b]
]

VerificationTest[
  Log[m] // changeLogPower[2]
  ,
  1/2 Log[m^2]
]

VerificationTest[
  -2 Log[m] + Log[s] //
    modify[{Log[m]}, changeLogPower[2]] //
    collectLogs // changeLogPower[-1]
  ,
  Log[s/m^2]
]

VerificationTest[
  a Conjugate[a] // {Identity, complexToAbs[a]} // Through
  ,
  {a Conjugate[a], Abs[a]^2}
]

VerificationTest[
  c a b Conjugate[a b] // {Identity, complexToAbs[a b]} // Through
  ,
  {c a b Conjugate[a b], c Abs[a b]^2}
]


EndTestSection[]
