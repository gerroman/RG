Needs["RG`Calculation`"];


test[1] :=
VerificationTest[(* #1 *)
  a + b x // pullIt[x]
  ,
  x (a/x + b)
]


test[2] :=
VerificationTest[(* #2 *)
  a x // pullIt[x]
  ,
  a x
]


test[3] :=
VerificationTest[(* #3 *)
  a + 3 x // pullIt[3]
  ,
  3(a/3 + x)
]


test[4] :=
VerificationTest[(* #4 *)
  1 + 2 + 3 b  + x/9 // pullIt[x]
  ,
  x (1/9 + 3 / x + (3 b) / x)
]


test[5] :=
VerificationTest[(* #5 *)
  a + b x + c // pullIt[{a, b}] // pullIt[{a, b}]
  ,
  a b (1/b + x / a + c / (a b))
]



