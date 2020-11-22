Needs["RG`Presentation`"];

test[1] := VerificationTest[(* #1 *)
  Equal[OverTilde[f][][x], f[x]]
  ,
  True	
]

test[2] := VerificationTest[(* #2 *)
  Equal[OverTilde[f][x][y], f[y, x]]
  ,
  True	
]

test[3] := VerificationTest[(* #3 *)
  x // OverTilde[f][y1, y2]
  ,
  f[x, y1, y2]
]

test[4] := VerificationTest[(* #4 *)
  UnderBar[f]
  ,
  HoldForm[f]
]

test[5] := VerificationTest[(* #5 *)
  1 + x // hold[{x}]
  ,
  1 + HoldForm[x]
]

test[6] := VerificationTest[(* #6 *)
  1 + x // hold[{1, x}]
  ,
  HoldForm[1] + HoldForm[x]
]

test[7] := VerificationTest[(* #7 *)
  1 + x // hold[_Integer]
  ,
  HoldForm[1] + x
]

test[8] := VerificationTest[(* #8 *)
  1 + x // hold[_Symbol]
  ,
  1 + HoldForm[x]
]

test[9] := VerificationTest[(* #9 *)
  1 + x // hold[_Symbol|_Integer]
  ,
  HoldForm[1] + HoldForm[x]
]
