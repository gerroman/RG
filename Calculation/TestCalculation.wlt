BeginTestSection["TestCalculation"]

Needs["RG`Calculation`"];

VerificationTest[(* #1 *)
	modify[x, f][x]
	,
	f[x]	
]

VerificationTest[(* #2 *)
	modify[x, f /* g][x]
	,
	g[f[x]]	
]

VerificationTest[(* #3 *)
	modify[Blank[Symbol], f][List[x, y]]
	,
	List[f[x], f[y]]	
]

VerificationTest[(* #4 *)
	modify[List[x], f][List[x, y]]
	,
	List[f[x], y]	
]

VerificationTest[(* #5 *)
	modify[List[Expand[Power[Plus[c, d], 2]]], RightComposition[Factor, ReplaceAll[Rule[c, 1]]]][Plus[Power[a, 2], Times[2, a, b], Power[b, 2], Power[c, 2], Times[2, c, d], Power[d, 2]]]
	,
	Plus[Power[a, 2], Times[2, a, b], Power[b, 2], Power[Plus[1, d], 2]]	
]

VerificationTest[(* #6 *)
  f[1, 2 a, 3 b] // pullFactors[{2}, f]
  ,
  2 f[1, a, 3 b]
]

VerificationTest[(* #7 *)
  f[1, 2 a, 3 b] // pullFactors[{2, 3}, f]
  ,
  6 f[1, a, b]
]

VerificationTest[(* #8 *)
  f[1, 2 a, 3 b] // pullFactors[_Integer, f]
  ,
  6 f[1, a, b]
]

VerificationTest[(* #9 *)
  a // fixedPoint[# + 1&, 3]
  ,
  a + 3
]

VerificationTest[(* #10 *)
  Hold[HoldForm[Hold[a]]] // release
  ,
  a
]

VerificationTest[(* #11 *)
  a^2 + b^2 + 2 a b + c^2 + 5 // groupIt[(a+b)^2]
  ,
  (a + b)^2 + c^2 + 5
]

VerificationTest[(* #12 *)
  1 + a x + 5 x + b x+ x^2 // factorIt[x]
  ,
  1 + x (a + 5 + b) + x^2
]

VerificationTest[(* #13 *)
  1 + a x + 5 x + x^2 // pullIt[x]
  ,
  x^2 (a/x + 5/x + 1/x^2 + 1)
]

VerificationTest[(* #14 *)
  1 + a x + 5 x + x^2 // pullIt[x, Identity, Plus, 1]
  ,
  x (a + 5 + 1/x + x)
]
VerificationTest[(* #15 *)
  1 + a x + 5 x + b x+ x^2 // factorIt[{x}]
  ,
  1 + x (a + 5 + b) + x^2
]

VerificationTest[(* #16 *)
  1 + a x + 5 x + x^2 // pullIt[{x}]
  ,
  x^2 (a/x + 5/x + 1/x^2 + 1)
]

VerificationTest[(* #17 *)
  1 + a x + 5 x + x^2 // pullIt[{x}, Identity, Plus, 1]
  ,
  x (a + 5 + 1/x + x)
]

VerificationTest[(* #18 *)
  powersPattern[{}]
  ,
  {1}
]
VerificationTest[(* #19 *)
  powersPattern[{a, b}]
  ,
  {a^(_.)*b^(_.), b^(_.), a^(_.), 1}
]

VerificationTest[(* #20 *)
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


VerificationTest[(* #21 *)
  rewriteIt[
    f == a^2 + b^2 + 2 a b,
    groupIt[(a + b)^2]
  ]
  ,
  f == (a + b)^2
]

VerificationTest[(* #22 *)
  b (a - b) // changeSign[a - b]
  ,
  - b (b - a)
]

VerificationTest[(* #23 *)
  (a - b) // changeSign[a - b]
  ,
  a - b
]

VerificationTest[(* #24 *)
  a == b // rewriteIt[ReplaceAll[b -> c]]
  ,
  a == c
]

VerificationTest[(* #25 *)
  a == b * a // rewriteIt@@ReplaceAll/@{a -> d, b -> c}
  ,
  d == c * a
]

VerificationTest[(* #26 *)
  a // rewriteIt[ReplaceAll[a -> b]]
  ,
  a == b
]

VerificationTest[(* #27 *)
  Sqrt[a^2] + Log[b^2] // powerExpand
  ,
  a + Log[b^2]
]

VerificationTest[(* #28 *)
  Log[a] + Log[b] // collectLogs
  ,
  Log[a b]
]

VerificationTest[(* #29 *)
  2 c Log[a] - 2 c Log[b] // collectLogs
  ,
  2 c Log[a/b]
]

VerificationTest[(* #30 *)
  Log[m] // changeLogPower[2]
  ,
  1/2 Log[m^2]
]

VerificationTest[(* #31 *)
  -2 Log[m] + Log[s] //
    modify[{Log[m]}, changeLogPower[2]] //
    collectLogs // changeLogPower[-1]
  ,
  Log[s/m^2]
]

VerificationTest[(* #32 *)
  a Conjugate[a] // {Identity, complexToAbs[a]} // Through
  ,
  {a Conjugate[a], Abs[a]^2}
]

VerificationTest[(* #33 *)
  c a b Conjugate[a b] // {Identity, complexToAbs[a b]} // Through
  ,
  {c a b Conjugate[a b], c Abs[a b]^2}
]


VerificationTest[(* #34 *)
  (a - b)^2 // changeSign[a - _]
  ,
  (b - a)^2
]

EndTestSection[]
