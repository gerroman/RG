BeginTestSection["TestBaseUtils"]

Needs["RG`BaseUtils`"];

VerificationTest[(* #1 *)
  assert[Greater[1, 2]]
  ,
  Null
  ,
  {Assert::asrtfl}
]

VerificationTest[(* #2 *)
  assert[Greater[2, 1]]
  ,
  Null	
]

VerificationTest[(* #3 *)
  carryFirst[f][y1, y2][x]
  ,
  f[x, y1, y2]
]

VerificationTest[(* #4 *)
  carryLast[f][y1, y2][x]
  ,
  f[y1, y2, x]
]

VerificationTest[(* #5 *)
  carryFirst[f][][x]
  ,
  f[x]
]

VerificationTest[(* #6 *)
  carryLast[f][][x]
  ,
  f[x]
]

EndTestSection[]
