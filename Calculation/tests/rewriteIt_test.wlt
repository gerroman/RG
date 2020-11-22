Needs["RG`Calculation`"];


test[1] :=
VerificationTest[(* #1 *)
  x // rewriteIt[Replace[x -> y]]
  ,
  x == y
];


test[2] :=
VerificationTest[(* #2 *)
  {x, y} // rewriteIt[ReplaceAll[_Symbol -> 1]]
  ,
  {x == 1, y == 1}
];

