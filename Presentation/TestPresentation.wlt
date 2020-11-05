BeginTestSection["TestPresentation"];

Needs["RG`Presentation`"];

VerificationTest[
  Equal[OverTilde[f][][x], f[x]]
  ,
  True	
]

VerificationTest[
  Equal[OverTilde[f][x][y], f[y, x]]
  ,
  True	
]

VerificationTest[
  x // OverTilde[f][y1, y2]
  ,
  f[x, y1, y2]
]

VerificationTest[
  UnderBar[f]
  ,
  HoldForm[f]
]

VerificationTest[
  1 + x // hold[{x}]
  ,
  1 + HoldForm[x]
]

VerificationTest[
  1 + x // hold[{1, x}]
  ,
  HoldForm[1] + HoldForm[x]
]

VerificationTest[
  1 + x // hold[_Integer]
  ,
  HoldForm[1] + x
]

VerificationTest[
  1 + x // hold[_Symbol]
  ,
  1 + HoldForm[x]
]

VerificationTest[
  1 + x // hold[_Symbol|_Integer]
  ,
  HoldForm[1] + HoldForm[x]
]

EndTestSection[]
