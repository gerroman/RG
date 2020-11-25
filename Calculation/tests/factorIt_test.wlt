Needs["RG`Calculation`"];


test[1] := VerificationTest[(* #1 *)
  1 + a x + 5 x + b x+ x^2 // factorIt[{x}]
  ,
  1 + x (a + 5 + b) + x^2
];


test[2] := VerificationTest[(* #2 *)
  1 + a x + 5 x + b x+ x^2 // factorIt[x]
  ,
  1 + x (a + 5 + b) + x^2
];


test[3] :=
VerificationTest[(* #3 *)
  1 + a x + 5 x + b x+ x^2 // factorItFast[x]
  ,
  1 + x (a + 5 + b) + x^2
]


test[4] :=
VerificationTest[(* #4 *)
  1 + a x // factorItFast[x]
  ,
  1 + a x
]

test[5] :=
VerificationTest[(* #5 *)
  f[(1 + c x + d x + x^2)]/(1 + a x + b x) // factorItFast[x, Infinity]
  ,
  f[(1 + x^2 + x (c + d))]/(1 + x (a+b))
]


test[6] :=
VerificationTest[(* #6 *)
  e^8 x + e^8 y // factorItFast[e^8]
  ,
  e^8 * (x + y)
]

test[7] :=
VerificationTest[(* #7 *)
  3 x - 3 y + 3 z// factorItFast[3, {0}]
  ,
  3(x - y + z)
]
