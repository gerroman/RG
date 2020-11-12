BeginTestSection["TestPresentation"];

Needs["RG`Presentation`"];

VerificationTest[(* #1 *)
  Equal[OverTilde[f][][x], f[x]]
  ,
  True	
]

VerificationTest[(* #2 *)
  Equal[OverTilde[f][x][y], f[y, x]]
  ,
  True	
]

VerificationTest[(* #3 *)
  x // OverTilde[f][y1, y2]
  ,
  f[x, y1, y2]
]

VerificationTest[(* #4 *)
  UnderBar[f]
  ,
  HoldForm[f]
]

VerificationTest[(* #5 *)
  1 + x // hold[{x}]
  ,
  1 + HoldForm[x]
]

VerificationTest[(* #6 *)
  1 + x // hold[{1, x}]
  ,
  HoldForm[1] + HoldForm[x]
]

VerificationTest[(* #7 *)
  1 + x // hold[_Integer]
  ,
  HoldForm[1] + x
]

VerificationTest[(* #8 *)
  1 + x // hold[_Symbol]
  ,
  1 + HoldForm[x]
]

VerificationTest[(* #9 *)
  1 + x // hold[_Symbol|_Integer]
  ,
  HoldForm[1] + HoldForm[x]
]

EndTestSection[]
