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

