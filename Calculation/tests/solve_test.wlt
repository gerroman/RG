Needs["RG`Calculation`"];


test[1] :=
VerificationTest[(* #1 *)
  solve[a + b[z] == 1, b[z]]
  ,
  {b[z] -> 1 - a}
]


test[2] :=
VerificationTest[(* #2 *)
  a + b[z] == 1 // solve[b[z]]
  ,
  {b[z] -> 1 - a}
]


test[3] :=
VerificationTest[(* #3 *)
  {x + y == 1, x - y == 3} // solve[{x, y}]
  ,
  {x -> 2, y -> -1}
]
