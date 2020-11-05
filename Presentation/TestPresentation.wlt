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

EndTestSection[]
