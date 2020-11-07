BeginTestSection["TestTraces"]


Needs["RG`Traces`"];


VerificationTest[
  tr[id] // traceCalc
  ,
  4
]

VerificationTest[
  tr[\[Gamma][\[Mu]]] // traceCalc
  ,
  0
]

VerificationTest[
  tr[Dot[\[Gamma][\[Mu]],\[Gamma][\[Nu]]]] // traceCalc
  ,
  4 * sp[\[Mu], \[Nu]]
]

VerificationTest[
  tr[Dot[\[Gamma][\[Mu]], \[Gamma][\[Nu]], \[Gamma][\[Rho]]]] // traceCalc
  ,
  0
]

VerificationTest[
  tr[Dot[\[Gamma][a], \[Gamma][b], \[Gamma][c], \[Gamma][d]]] // traceCalc
  ,
  4 sp[a, b] sp[c, d]
  - 4 sp[a, c] sp[b, d]
  + 4 sp[a, d] sp[b, c]
]

VerificationTest[
  Block[{
      traceScalars = {m},
      rule`sp = {
        sp[p1, p1] -> m^2,
        sp[p2, p2] -> m^2
      }
    },
    (1/4) * tr[Dot[(\[Gamma][p1] + m id), \[Gamma][\[Mu]], (\[Gamma][p2] + m id),  \[Gamma][\[Nu]]]] //
      traceCalc // ReplaceAll[rule`sp] // factorIt[_sp]
  ]
  ,
  sp[\[Mu], \[Nu]](m^2 - sp[p1, p2]) + sp[p1, \[Mu]] sp[p2, \[Nu]] + sp[p1, \[Nu]] sp[p2, \[Mu]]
]


VerificationTest[
  Conjugate[Dot[a, b]] // diracConjugate
  ,
  Conjugate[Dot[a, b]]
]

VerificationTest[
  Conjugate[bar`u[p1].u[p2]] // diracConjugate
  ,
  bar`u[p2].u[p1]
]

VerificationTest[
  Conjugate[Dot[bar`u[p1], \[Gamma][\[Mu]], u[p2]]] // diracConjugate
  ,
  Dot[bar`u[p2], \[Gamma][\[Mu]], u[p1]]
]

VerificationTest[
  Conjugate[Dot[bar`v[p1], \[Gamma][\[Mu]], \[Gamma][\[Nu]], u[p2]]] // diracConjugate
  ,
  Dot[bar`u[p2], \[Gamma][\[Nu]], \[Gamma][\[Mu]], v[p1]]
]


VerificationTest[
  Block[{p, traceScalars={m}},
    p /: mass[p] = m;
    bar`u[p].u[p] // spinSum[p] // traceCalc
  ]
  ,
  4*m
]

VerificationTest[
  Block[{p, traceScalars={m}},
    p /: mass[p] = m;
    bar`v[p].v[p] // spinSum[p] // traceCalc
  ]
  ,
  -4*m
]

VerificationTest[
  bar`u[p1].u[p2] * bar`u[p2].u[p1] // spinSum[p2]
  ,
  bar`u[p1].(\[Gamma][p2] + mass[p2] * id).u[p1]
]

VerificationTest[
  bar`u[p1].u[p2] * bar`u[p2].u[p1] // spinSum[p1]
  ,
  bar`u[p2].(\[Gamma][p1] + mass[p1] * id).u[p2]
]

VerificationTest[
  bar`u[p1].v[p2] * bar`v[p2].u[p1] // spinSum[]
  ,
  tr[
    (\[Gamma][p1] + mass[p1] * id)
    .(\[Gamma][p2] - mass[p2] * id)
  ]
]

EndTestSection[]