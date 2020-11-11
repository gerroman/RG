BeginTestSection["TestKinematics"]

Needs["RG`Kinematics`"];

VerificationTest[
  sp[a] == sp[a, a]
  ,
  True
]

VerificationTest[
  sp[2a, -3b]
  ,
  (-6) sp[a, b]
]

VerificationTest[
  energy[p + q]
  ,
  energy[p] + energy[q]
]

VerificationTest[
  energy[x p] // {Identity, pullFactors[x, energy]} // Through
  ,
  {energy[x p], x energy[p]}
]

VerificationTest[
  momentum[p + q]
  ,
  momentum[p] + momentum[q]
]

VerificationTest[
  momentum[x p] // {Identity, pullFactors[x, momentum]} // Through
  ,
  {momentum[x p], x momentum[p]}
]

VerificationTest[
  {abs[1], abs[{a,b}], Abs[abs[momentum[p]]], abs[3 momentum[p]]}
  ,
  {1, abs[{a,b}], abs@momentum[p], 3 abs[momentum[p]]}
]


VerificationTest[
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


VerificationTest[
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


VerificationTest[
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

VerificationTest[
  energy[p] // replaceEnergy[p, All]
  ,
  Sqrt[abs@momentum[p]^2 + mass[p]^2]
]

VerificationTest[
  mass[p] // replaceMass[p, All]
  ,
  Sqrt[energy[p]^2 - abs@momentum[p]^2]
]

VerificationTest[
  abs@momentum[p] // replaceMomentum[p, All]
  ,
  Sqrt[energy[p]^2 - mass[p]^2]
]

VerificationTest[
  setInvariants[
    {p1, p2, -p3, -p4},
    {m1, m2, m3, m4},
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

VerificationTest[
  theta[a, b] == theta[b, a]
  ,
  True
]

VerificationTest[
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

VerificationTest[
  Normal[getKinematicsCMS[{p1, p2}, {p3, p4}, {s, \[Theta]}]]
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


VerificationTest[Block[{p1, p2, p3, p4},
  p1 /: mass[p1] = 0;
  p2 /: mass[p2] = 0;
  p3 /: mass[p3] = 0;
  p4 /: mass[p4] = 0;
  Normal[getKinematicsCMS[{p1, p2}, {p3, p4}, {s, \[Theta]}]]
  ]
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

EndTestSection[]
