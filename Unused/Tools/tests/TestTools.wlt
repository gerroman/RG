Get["RG`Tools`"]


VerificationTest[
  1 + x // hold[x, y]
  ,
  1 + Hold[x]
]


VerificationTest[
  1 + Hold[Hold[x]y] // release[x y, x]
  ,
  1 + x y
]


VerificationTest[
  Sqrt[x^2] // powerExpand[x]
  ,
  x
]


VerificationTest[
  1 + x a y + x b + x c y // factorIt[x, y]
  ,
  1 + x (y(a + c) + b)
]


VerificationTest[
  1 + x a y + x b + x c y // pullIt[x, y]
  ,
  x y (1/(x y) + a + c + b / y)
]


VerificationTest[
  (x - 1)/(y - 1) // changeSign[x - 1, y - 1],
  (1 - x)/(1 - y)
]


VerificationTest[
  (x - 1)^2/(y - 1)^3 // changeSign[x - 1, y - 1],
  (-1)(1 - x)^2/(1 - y)^3
]


VerificationTest[
  eq[Sin[x]],
  HoldForm[Sin[x]]==Sin[x]
]


VerificationTest[
  1 + a x + a y + b (x + y) // groupIt[a (x + y)] // groupIt[(x + y)(a + b), hold[x + y]/*Expand/*release[x + y]]
  ,
  1 + (a + b)(x + y)
]


VerificationTest[
  1 + a x + a y + b (x + y) // modify[a x + a y + b (x + y), Factor]
  ,
  1 + (a + b)(x + y)
]


VerificationTest[
  1 + a x + a y + b (x + y) // cases[_Symbol]
  ,
  {a, b, x, y}
]
