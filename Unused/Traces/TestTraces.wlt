Needs["RG`Traces`"];


test[1] := VerificationTest[(* #1 *)
  tr[id] // traceCalc
  ,
  4
]

test[2] := VerificationTest[(* #2 *)
  tr[\[Gamma][\[Mu]]] // traceCalc
  ,
  0
]

test[3] := VerificationTest[(* #3 *)
  tr[Dot[\[Gamma][\[Mu]],\[Gamma][\[Nu]]]] // traceCalc
  ,
  4 * sp[\[Mu], \[Nu]]
]

test[4] := VerificationTest[(* #4 *)
  tr[Dot[\[Gamma][\[Mu]], \[Gamma][\[Nu]], \[Gamma][\[Rho]]]] // traceCalc
  ,
  0
]

test[5] := VerificationTest[(* #5 *)
  tr[Dot[\[Gamma][a], \[Gamma][b], \[Gamma][c], \[Gamma][d]]] // traceCalc
  ,
  4 sp[a, b] sp[c, d]
  - 4 sp[a, c] sp[b, d]
  + 4 sp[a, d] sp[b, c]
]

test[6] := VerificationTest[(* #6 *)
  Block[{
      traceScalars = {m},
      rule`sp = {
        sp[p1, p1] -> m^2,
        sp[p2, p2] -> m^2
      }
    },
    (1/4) * tr[Dot[(\[Gamma][p1] + m id), \[Gamma][\[Mu]], (\[Gamma][p2] + m id),  \[Gamma][\[Nu]]]] //
      traceCalc // Expand // ReplaceAll[rule`sp] // factorIt[_sp]
  ]
  ,
  sp[\[Mu], \[Nu]](m^2 - sp[p1, p2]) + sp[p1, \[Mu]] sp[p2, \[Nu]] + sp[p1, \[Nu]] sp[p2, \[Mu]]
]


test[7] := VerificationTest[(* #7 *)
  Conjugate[Dot[a, b]] // diracConjugate
  ,
  Conjugate[Dot[a, b]]
]

test[8] := VerificationTest[(* #8 *)
  Conjugate[bar`u[p1].u[p2]] // diracConjugate
  ,
  bar`u[p2].u[p1]
]

test[9] := VerificationTest[(* #9 *)
  Conjugate[Dot[bar`u[p1], \[Gamma][\[Mu]], u[p2]]] // diracConjugate
  ,
  Dot[bar`u[p2], \[Gamma][\[Mu]], u[p1]]
]

test[10] := VerificationTest[(* #10 *)
  Conjugate[Dot[bar`v[p1], \[Gamma][\[Mu]], \[Gamma][\[Nu]], u[p2]]] // diracConjugate
  ,
  Dot[bar`u[p2], \[Gamma][\[Nu]], \[Gamma][\[Mu]], v[p1]]
]


test[11] := VerificationTest[(* #11 *)
  Block[{p, traceScalars={m}},
    p /: mass[p] = m;
    bar`u[p].u[p] // spinSum[p] // traceCalc
  ]
  ,
  4*m
]

test[12] := VerificationTest[(* #12 *)
  Block[{p, traceScalars={m}},
    p /: mass[p] = m;
    bar`v[p].v[p] // spinSum[p] // traceCalc
  ]
  ,
  -4*m
]

test[13] := VerificationTest[(* #13 *)
  bar`u[p1].u[p2] * bar`u[p2].u[p1] // spinSum[p2]
  ,
  bar`u[p1].(\[Gamma][p2] + mass[p2] * id).u[p1]
]

test[14] := VerificationTest[(* #14 *)
  bar`u[p1].u[p2] * bar`u[p2].u[p1] // spinSum[p1]
  ,
  bar`u[p2].(\[Gamma][p1] + mass[p1] * id).u[p2]
]

test[15] := VerificationTest[(* #15 *)
  bar`u[p1].v[p2] * bar`v[p2].u[p1] // spinSum[]
  ,
  tr[
    (\[Gamma][p1] + mass[p1] * id)
    .(\[Gamma][p2] - mass[p2] * id)
  ]
]

test[16] := VerificationTest[(* #16 *)
  {sp[a, b] * sp[b, c], sp[a, b]^2, sp[b, b]} // contractLorentzIndices[{b}]
  ,
  {sp[a, c], sp[a, a], 4}
]

test[17] := VerificationTest[(* #17 *)
  setLorentzIndex[\[Mu], \[Nu]];
  sp[a, \[Nu]] sp[b, \[Mu]] sp[\[Mu], \[Nu]] // contractLorentzIndices[]
  ,
  sp[a, b]
]
