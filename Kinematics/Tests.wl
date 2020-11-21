Needs["RG`Kinematics`"];


test[1] := VerificationTest[(* #1 *)
  sp[a] == sp[a, a]
  ,
  True
]


test[2] := VerificationTest[(* #2 *)
  sp[2a, -3b]
  ,
  (-6) sp[a, b]
]


test[3] := VerificationTest[(* #3 *)
  energy[p + q]
  ,
  energy[p] + energy[q]
]


test[4] := VerificationTest[(* #4 *)
  energy[x p] // {Identity, pullFactors[x, energy]} // Through
  ,
  {energy[x p], x energy[p]}
]


test[5] := VerificationTest[(* #5 *)
  momentum[p + q]
  ,
  momentum[p] + momentum[q]
]


test[6] := VerificationTest[(* #6 *)
  momentum[x p] // {Identity, pullFactors[x, momentum]} // Through
  ,
  {momentum[x p], x momentum[p]}
]


test[7] := VerificationTest[(* #7 *)
  {abs[1], abs[{a,b}], Abs[abs[momentum[p]]], abs[3 momentum[p]]}
  ,
  {1, abs[{a,b}], abs@momentum[p], 3 abs[momentum[p]]}
]


test[8] := VerificationTest[(* #8 *)
  {
    energy[p],
    energy[p]^2,
    energy[p]^3,
    energy[p]^(-1),
    energy[p]^(-2)
  } // replaceEnergy[p]
  ,
  {
    energy[p],
    (abs@momentum[p]^2 + mass[p]^2),
    energy[p] * (abs@momentum[p]^2 + mass[p]^2),
    energy[p] / (abs@momentum[p]^2 + mass[p]^2), (*!*)
    1/(abs@momentum[p]^2 + mass[p]^2)
  }
]


test[9] := VerificationTest[(* #9 *)
  {
    abs@momentum[p],
    abs@momentum[p]^2,
    abs@momentum[p]^3,
    abs@momentum[p]^(-1),
    abs@momentum[p]^(-2)
  } // replaceMomentum[p]
  ,
  {
    abs@momentum[p],
    (energy[p]^2 - mass[p]^2),
    abs@momentum[p] * (energy[p]^2 - mass[p]^2),
    abs@momentum[p] / (energy[p]^2 - mass[p]^2), (*!*)
    1/(energy[p]^2 - mass[p]^2)
  }
]


test[10] := VerificationTest[(* #10 *)
  {
    mass[p],
    mass[p]^2,
    mass[p]^3,
    mass[p]^(-1),
    mass[p]^(-2)
  } // replaceMass[p]
  ,
  {
    mass[p],
    (-abs@momentum[p]^2 + energy[p]^2),
    mass[p] * (-abs@momentum[p]^2 + energy[p]^2),
    mass[p] / (-abs@momentum[p]^2 + energy[p]^2), (*!*)
    1/(-abs@momentum[p]^2 + energy[p]^2)
  }
]


test[11] := VerificationTest[(* #11 *)
  energy[p] // replaceEnergy[p, All]
  ,
  Sqrt[abs@momentum[p]^2 + mass[p]^2]
]


test[12] := VerificationTest[(* #12 *)
  mass[p] // replaceMass[p, All]
  ,
  Sqrt[energy[p]^2 - abs@momentum[p]^2]
]


test[13] := VerificationTest[(* #13 *)
  abs@momentum[p] // replaceMomentum[p, All]
  ,
  Sqrt[energy[p]^2 - mass[p]^2]
]


test[14] := VerificationTest[(* #14 *)
  setInvariants[
    {{p1, p2}, {p3, p4}},
    {{m1, m2}, {m3, m4}},
    {},
    {
      sp[p1, p2] -> (s - m1^2 - m2^2) / 2,
      sp[p1, p3] -> (m1^2 + m3^2 - t) / 2
    }
  ]
  ,
  {
    sp[p1, p1] -> m1^2,
    sp[p1, p2] -> s/2 - m1^2/2 - m2^2/2,
    sp[p1, p3] -> m1^2/2 + m3^2/2 - t/2,
    sp[p1, p4] -> -m2^2/2 - m3^2/2 + s/2 + t/2,
    sp[p2, p2] -> m2^2,
    sp[p2, p3] -> -m1^2/2 - m4^2/2 + s/2 + t/2,
    sp[p2, p4] -> m2^2/2 + m4^2/2 - t/2,
    sp[p3, p3] -> m3^2,
    sp[p3, p4] -> -m3^2/2 - m4^2/2 + s/2,
    sp[p4, p4] -> m4^2
  }
]


test[15] := VerificationTest[(* #15 *)
  theta[a, b] == theta[b, a]
  ,
  True
]


test[16] := VerificationTest[(* #16 *)
  (
    energy[a] = energy[b] = \[ScriptCapitalE];
    abs@momentum[a] = abs@momentum[b] = \[ScriptP];
    mass[a] = mass[b] = m;
    theta[momentum[a], momentum[b]] = \[Pi];
    sp[a + b] // modify[_sp, Distribute] // expandScalarProduct // Expand
  )
  ,
  4 \[ScriptCapitalE]^2
]


test[17] := VerificationTest[(* #17 *)
  Normal[getKinematicsCMS[{{p1, p2}, {p3, p4}}, {s, \[Theta]}]]
  ,
  {
    abs[momentum[p1]] -> pcms,
    abs[momentum[p2]] -> pcms,
    abs[momentum[p3]] -> prime`pcms,
    abs[momentum[p4]] -> prime`pcms,
    energy[p1] -> Sqrt[mass[p1]^2 + pcms^2],
    energy[p2] -> Sqrt[mass[p2]^2 + pcms^2],
    energy[p3] -> Sqrt[mass[p3]^2 + prime`pcms^2],
    energy[p4] -> Sqrt[mass[p4]^2 + prime`pcms^2],
    theta[momentum[p1], momentum[p2]] -> Pi,
    theta[momentum[p1], momentum[p3]] -> θ,
    theta[momentum[p1], momentum[p4]] -> Pi - θ,
    theta[momentum[p2], momentum[p3]] -> Pi - θ,
    theta[momentum[p2], momentum[p4]] -> θ,
    theta[momentum[p3], momentum[p4]] -> Pi,
    pcms^2 -> ((s - (mass[p1] - mass[p2])^2)*(s - (mass[p1] + mass[p2])^2))/(4*s),
    prime`pcms^2 -> ((s - (mass[p3] - mass[p4])^2)*(s - (mass[p3] + mass[p4])^2))/(4*s)
  }
]


Block[
  {p1, p2, p3, p4, s, \[Theta]},
  p1 /: mass[p1] = 0;
  p2 /: mass[p2] = 0;
  p3 /: mass[p3] = 0;
  p4 /: mass[p4] = 0;

test[18] :=   VerificationTest[(* #18 *)
    Normal[getKinematicsCMS[{{p1, p2}, {p3, p4}}, {s, \[Theta]}]]
    ,
    {
      abs[momentum[p1]] -> pcms,
      abs[momentum[p2]] -> pcms,
      abs[momentum[p3]] -> prime`pcms,
      abs[momentum[p4]] -> prime`pcms,
      energy[p1] -> Sqrt[pcms^2],
      energy[p2] -> Sqrt[pcms^2],
      energy[p3] -> Sqrt[prime`pcms^2],
      energy[p4] -> Sqrt[prime`pcms^2],
      theta[momentum[p1], momentum[p2]] -> Pi,
      theta[momentum[p1], momentum[p3]] -> θ,
      theta[momentum[p1], momentum[p4]] -> Pi - θ,
      theta[momentum[p2], momentum[p3]] -> Pi - θ,
      theta[momentum[p2], momentum[p4]] -> θ,
      theta[momentum[p3], momentum[p4]] -> Pi,
      pcms^2 -> s/4,
      prime`pcms^2 -> s/4
    }
  ]
]


test[19] := VerificationTest[(* #19 *)
  Block[
    {
      l1, p1,l2, p2, rules, q, r, m, M, s, t, u
    },
    With[{momenta = {{l1, p1}, {l2, p2}}},
    l1 /: mass[l1] = m;
    l2 /: mass[l2] = m;
    p1 /: mass[p1] = M;
    p2 /: mass[p2] = M;
    rules = setInvariants[
      momenta,
      mass /@ Join@@momenta,
      {r -> l1 + p1, q -> l1 - l2},
      {sp[r] -> s, sp[q] -> t}
    ];
    getMandelstam[momenta, {s, t, u}] //
      {Identity, modify[_sp, Distribute] /* ReplaceAll[rules] /* Expand} // Through
    ]
  ]
  ,
  {
    {
      s + t + u == 2m^2 + 2M^2,
      s == sp[l1 + p1],
      t == sp[l1 - l2],
      u == sp[l1 - p2]
    },
    {
      s + t + u == 2m^2 + 2M^2,
      True,
      True,
      u == 2m^2 + 2M^2 - s - t
    }
  }
]


test[20] := VerificationTest[(* #20 *)
  Block[
    {
      l1, p1,l2, p2, rules
    },
    With[
      {
        momenta = {{l1, p1}, {l2, p2}}
      },
      l1 /: mass[l1] = 0;
      p1 /: mass[p1] = 0;
      l2 /: mass[l2] = m;
      p2 /: mass[p2] = m;
      rules = getKinematicsCMS[momenta, {s, \[Theta]}];
      getMandelstam[momenta, {s, t, u}] //
        {
	  Identity,
	  modify[_sp, Distribute] /* expandScalarProduct /*
	  ReplaceAll[rules] /* Expand /* powerExpand /* factorIt[{(-2)*pcms}]
	} // Through
    ]
  ]
  ,
  {
    {
      s + t + u == 2*m^2,
      s == sp[l1 + p1],
      t == sp[l1 - l2],
      u == sp[l1 - p2]
    },
    {
      s + t + u == 2*m^2,
      s == 4*pcms^2,
      t == m^2 - 2 * pcms * (Sqrt[m^2 + prime`pcms^2] - prime`pcms*Cos[θ]),
      u == m^2 - 2 * pcms * (Sqrt[m^2 + prime`pcms^2] + prime`pcms*Cos[θ])
    }
  }
]


test[21] := VerificationTest[(* #21 *)
  setInvariants[
    {{l, p}, {l1, p1}},
    {},
    {},
    {
      sp[l, p] -> mass[p] energy[l],
      sp[l, l1] -> (mass[l]^2 + mass[l1]^2 - t) / 2
    }
  ]
  ,
  {
    sp[l, l] -> mass[l]^2,
    sp[l, l1] -> -t/2 + mass[l]^2/2 + mass[l1]^2/2,
    sp[l, p] -> energy[l]*mass[p],
    sp[l, p1] -> t/2 + mass[l]^2/2 - mass[l1]^2/2 + energy[l]*mass[p],
    sp[l1, l1] -> mass[l1]^2,
    sp[l1, p] -> t/2 + energy[l]*mass[p] + mass[p]^2/2 - mass[p1]^2/2,
    sp[l1, p1] -> mass[l]^2/2 - mass[l1]^2/2 + energy[l]*mass[p] + mass[p]^2/2 - mass[p1]^2/2,
    sp[p, p] -> mass[p]^2,
    sp[p, p1] -> -t/2 + mass[p]^2/2 + mass[p1]^2/2,
    sp[p1, p1] -> mass[p1]^2
  }
]


Block[{p1, p2, p3, p4, k, \[Kappa], \[CapitalDelta], rules, \[Nu], m, M, \[Lambda]},
test[22] := VerificationTest[(* #22 *)
  (
    rules = setInvariants[
      {{p1, p2}, {p3, p4, k}},
      {{m, M}, {m, M, \[Lambda]}},
      {},
      {
        sp[p1, p2] -> \[Nu],
        sp[k, p2] -> \[Kappa][2] + \[Lambda]^2 / 2,
        sp[k, p4] -> \[Kappa][4] - \[Lambda]^2 / 2,
        sp[p1, p3] -> (\[CapitalDelta][1]^2 + 2 m^2) / 2,
        sp[k, p1] -> \[Kappa] - (\[Kappa][2] + \[Lambda]^2 / 2) + \[Lambda]^2 /2
      }
    ];
    {
      sp[p1 + p2],
      sp[p1 + p2 - k],
      sp[p2 - k],
      sp[p1 - p3],
      sp[p4 + k]
    } // modify[_sp, Distribute] // ReplaceAll[rules] // Expand
  )
  ,
  {
    2 \[Nu] + m^2 + M^2,
    2 \[Nu]  + m^2 + M^2 - 2 \[Kappa],
    M^2 - 2 \[Kappa][2],
    - \[CapitalDelta][1]^2,
    2 \[Kappa][4] + M^2
  }
  ]
]


test[23] := VerificationTest[(* #23 *)
  setInvariants[
    {{p1, p2}, {p3, p4, k}}
    , {m, M, m, M, \[Lambda]}
    , {qe -> p1 - p3, qp -> p2 - p4}
    , {sp[qe], sp[qp], sp[p1, p2], sp[p2, k] -> \[Kappa][2], sp[p4, k] -> \[Kappa][4]}
  ]
  ,
  {}
]


test[24] := VerificationTest[(* #24 *)
  setInvariants[
    {{p1, p2}, {p3, p4}},
    {{m1, m2}, {m3, m4}},
    {q -> p1 - p3},
    {
      sp[p1, p2] -> (s - m1^2 - m2^2) / 2,
      sp[q] -> t
    }
  ]
  ,
  {
    sp[p1, p1] -> m1^2
    , sp[p1, p2] -> -m1^2/2 - m2^2/2 + s/2
    , sp[p1, p3] -> m1^2/2 + m3^2/2 - t/2
    , sp[p1, p4] -> -m2^2/2 - m3^2/2 + s/2 + t/2
    , sp[p1, q] -> m1^2/2 - m3^2/2 + t/2
    , sp[p2, p2] -> m2^2
    , sp[p2, p3] -> -m1^2/2 - m4^2/2 + s/2 + t/2
    , sp[p2, p4] -> m2^2/2 + m4^2/2 - t/2
    , sp[p2, q] -> -m2^2/2 + m4^2/2 - t/2
    , sp[p3, p3] -> m3^2
    , sp[p3, p4] -> -m3^2/2 - m4^2/2 + s/2
    , sp[p3, q] -> m1^2/2 - m3^2/2 - t/2
    , sp[p4, p4] -> m4^2
    , sp[p4, q] -> -m2^2/2 + m4^2/2 + t/2
    , sp[q, q] -> t
  }
];

test[25] := VerificationTest[(* #25 *)
  setInvariants[
    {{l1, p1}, {l2, p2}}
    , {{m, M}, {m, M}}
    , {r -> l1 + p1, q -> l1 - l2}
    , {sp[r] -> s, sp[q] -> t}
  ]
  ,
  {
    sp[l1, l1] -> m^2
    , sp[l1, l2] -> m^2 - t/2
    , sp[l1, p1] -> -m^2/2 - M^2/2 + s/2
    , sp[l1, p2] -> -m^2/2 - M^2/2 + s/2 + t/2
    , sp[l1, q] -> t/2
    , sp[l1, r] -> m^2/2 - M^2/2 + s/2
    , sp[l2, l2] -> m^2
    , sp[l2, p1] -> -m^2/2 - M^2/2 + s/2 + t/2
    , sp[l2, p2] -> -m^2/2 - M^2/2 + s/2
    , sp[l2, q] -> -t/2
    , sp[l2, r] -> m^2/2 - M^2/2 + s/2
    , sp[p1, p1] -> M^2
    , sp[p1, p2] -> M^2 - t/2
    , sp[p1, q] -> -t/2
    , sp[p1, r] -> -m^2/2 + M^2/2 + s/2
    , sp[p2, p2] -> M^2
    , sp[p2, q] -> t/2
    , sp[p2, r] -> -m^2/2 + M^2/2 + s/2
    , sp[q, q] -> t
    , sp[q, r] -> 0
    , sp[r, r] -> s
  }
]