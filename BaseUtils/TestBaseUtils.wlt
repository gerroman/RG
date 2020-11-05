BeginTestSection["TestBaseUtils"]

Needs["RG`BaseUtils`"];

VerificationTest[
  assert[Greater[1, 2]]
  ,
  Null
  ,
  {Assert::asrtfl}
]

VerificationTest[
  assert[Greater[2, 1]]
  ,
  Null	
]

VerificationTest[
  carryFirst[f][y1, y2][x]
  ,
  f[x, y1, y2]
]

VerificationTest[
  carryLast[f][y1, y2][x]
  ,
  f[y1, y2, x]
]

VerificationTest[
  carryFirst[f][][x]
  ,
  f[x]
]

VerificationTest[
  carryLast[f][][x]
  ,
  f[x]
]

EndTestSection[]
