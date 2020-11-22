Needs["RG`BaseUtils`"];


test[1] := VerificationTest[(* #1 *)
  assert[Greater[1, 2]]
  ,
  Null
  ,
  {Assert::asrtfl}
];


test[2] := VerificationTest[(* #2 *)
  assert[Greater[2, 1]]
  ,
  Null	
];


test[3] := VerificationTest[(* #3 *)
  carryFirst[f][y1, y2][x]
  ,
  f[x, y1, y2]
];


test[4] := VerificationTest[(* #4 *)
  carryLast[f][y1, y2][x]
  ,
  f[y1, y2, x]
];


test[5] := VerificationTest[(* #5 *)
  carryFirst[f][][x]
  ,
  f[x]
];


test[6] := VerificationTest[(* #6 *)
  carryLast[f][][x]
  ,
  f[x]
];

