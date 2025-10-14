Needs["QED`StandardGammaMatrices`", "StandardGammaMatrices.m"];


$Assumptions = 0<=\[Theta]<=Pi && 0<=\[Phi]<=2Pi && \[CurlyEpsilon] > m >= 0 && p >= 0 && \[CurlyEpsilon] >= p
rule`p2 = {p^2 -> \[CurlyEpsilon]^2 - m^2}


\[Gamma] = DiracGammaMatrix
\[Sigma] = PauliSigmaMatrix
id = DiracId
\[CurlyPhi] = PhiSpinor
\[Chi] = ChiSpinor


(* Definition of gamma^5 *)
\[Gamma][5] == ArrayFlatten[{{PauliZero, PauliId}, {PauliId, PauliZero}}]


(* Definition of basis *)
{
DiracBasis[[1]] == id,
DiracBasis[[2]] == \[Gamma][5],
DiracBasis[[3]] == \[Gamma][0],
DiracBasis[[4]] == \[Gamma][1],
DiracBasis[[5]] == \[Gamma][2],
DiracBasis[[6]] == \[Gamma][3],
DiracBasis[[7]] == (+I) \[Gamma][0].\[Gamma][5],
DiracBasis[[8]] == (+I) \[Gamma][1].\[Gamma][5],
DiracBasis[[9]] == (+I) \[Gamma][2].\[Gamma][5],
DiracBasis[[10]] == (+I) \[Gamma][3].\[Gamma][5],
DiracBasis[[11]] == DiracSigmaMatrix[0,1],
DiracBasis[[12]] == DiracSigmaMatrix[0,2],
DiracBasis[[13]] == DiracSigmaMatrix[0,3],
DiracBasis[[14]] == DiracSigmaMatrix[1,2],
DiracBasis[[15]] == DiracSigmaMatrix[1,3],
DiracBasis[[16]] == DiracSigmaMatrix[2,3]
}


(* 4x4 basis normalization *)
Outer[(1/4) * Tr[#1.#2]&, DiracBasis, DiracIdxLoweringCoeffs * DiracBasis, 1] == IdentityMatrix[16]


(* 2x2 basis normalization *)
Outer[(1/2) * Tr[#1.#2]&, PauliBasis, PauliBasis, 1] == IdentityMatrix[4]


(* charge conjugation of Pauli spinors*)
ChiSpinor[{\[Theta], \[Phi]}, (+1)] == PauliUCharge.Conjugate[PhiSpinor[{\[Theta], \[Phi]}, (+1)]] // Simplify
ChiSpinor[{\[Theta], \[Phi]}, (-1)] == PauliUCharge.Conjugate[PhiSpinor[{\[Theta], \[Phi]}, (-1)]] // Simplify


(* charge conjugation of Dirac spinors*)
With[{chi=ChiSpinor[{\[Theta], \[Phi]}, +1], phi=PhiSpinor[{\[Theta], \[Phi]}, +1], p4={\[CurlyEpsilon], 0, 0, p}},
  VSpinor[p4, m, chi] == DiracUCharge.Conjugate[USpinor[p4, m, phi]]
] // Simplify


With[{chi=ChiSpinor[{\[Theta], \[Phi]}, -1], phi=PhiSpinor[{\[Theta], \[Phi]}, -1], p4={\[CurlyEpsilon], 0, 0, p}},
  VSpinor[p4, m, chi] == DiracUCharge.Conjugate[USpinor[p4, m, phi]]
] // Simplify


{
(* Dirac Uc matrix *)
DiracUCharge == (+I) \[Gamma][2],
(* Uc is real *)
DiracUCharge\[Conjugate]==DiracUCharge,
(* Uc is symmetric *)
DiracUCharge\[Transpose]==DiracUCharge,
(* Uc is hermitian *)
DiracUCharge\[Conjugate]\[Transpose] == DiracUCharge,
(* Uc is unitary *)
DiracUCharge.(DiracUCharge\[Conjugate]\[Transpose]) == id
}


{
(* Pauli uc matrix *)
PauliUCharge == (-I) \[Sigma][2],
(* uc is real *)
PauliUCharge\[Conjugate]==PauliUCharge,
(* uc is anti-symmetric *)
PauliUCharge\[Transpose]==(-1)PauliUCharge,
(* uc is anti-hermitian *)
PauliUCharge\[Conjugate]\[Transpose] == (-1)PauliUCharge,
(* uc is unitary *)
PauliUCharge.(PauliUCharge\[Conjugate]\[Transpose]) == PauliId
}


(* Uc.gamma^mu.Uc == (-1) Conjugate[(gamma^mu)] *)
Array[
DiracUCharge.\[Gamma][#].DiracUCharge == (-1) * \[Gamma][#]\[Conjugate]&
,4,0
]

(* electron density matrix *)
With[{u = USpinor[{\[CurlyEpsilon], 0, 0, p}, m, \[CurlyPhi][{0,0}, +1]]},
With[{
  \[Rho] = Outer[Times, u, Conjugate[u].\[Gamma][0]],
  hp = \[CurlyEpsilon] * \[Gamma][0] - p * \[Gamma][3],
  ha = +p/m * \[Gamma][0] - \[CurlyEpsilon]/m * \[Gamma][3]
},
\[Rho] == (1 / 2) * (hp + m id).(id - ha.\[Gamma][5])
]
] // Simplify // ReplaceAll[rule`p2]
With[{u = USpinor[{\[CurlyEpsilon], 0, 0, p}, m, \[CurlyPhi][{0,0}, -1]]},
With[{
  \[Rho] = Outer[Times, u, Conjugate[u].\[Gamma][0]],
  hp = \[CurlyEpsilon] * \[Gamma][0] - p * \[Gamma][3],
  ha = -p/m * \[Gamma][0] + \[CurlyEpsilon]/m * \[Gamma][3]
},
\[Rho] == (1 / 2) * (hp + m id).(id - ha.\[Gamma][5])
]
] // Simplify // ReplaceAll[rule`p2]


(* positron density matrix *)
With[{v = VSpinor[{\[CurlyEpsilon], 0, 0, p}, m, \[Chi][{0, 0}, +1]]},
With[{
  \[Rho] = Outer[Times, v, Conjugate[v].\[Gamma][0]],
  hp = \[CurlyEpsilon] * \[Gamma][0] - p * \[Gamma][3],
  ha = +p/m * \[Gamma][0] - \[CurlyEpsilon]/m * \[Gamma][3]
},
\[Rho] == (1 / 2) * (hp - m id).(id - ha.\[Gamma][5])
]
] // Simplify // ReplaceAll[rule`p2]
With[{v = VSpinor[{\[CurlyEpsilon], 0, 0, p}, m, \[Chi][{0, 0}, -1]]},
With[{
  \[Rho] = Outer[Times, v, Conjugate[v].\[Gamma][0]],
  hp = \[CurlyEpsilon] * \[Gamma][0] - p * \[Gamma][3],
  ha = -p/m * \[Gamma][0] + \[CurlyEpsilon]/m * \[Gamma][3]
},
\[Rho] == (1 / 2) * (hp - m id).(id - ha.\[Gamma][5])
]
] // Simplify // ReplaceAll[rule`p2]


(* electron average spin *)
With[{phi = \[CurlyPhi][{\[Theta], \[Phi]}, +1]},
ToBasis[Outer[Times, phi, Conjugate[phi]]]
== (1/2) {1, Sin[\[Theta]]*Cos[\[Phi]], Sin[\[Theta]]*Sin[\[Phi]], Cos[\[Theta]]}
] // FullSimplify
With[{phi = \[CurlyPhi][{\[Theta], \[Phi]}, -1]},
ToBasis[Outer[Times, phi, Conjugate[phi]]]
== (1/2) {1, -Sin[\[Theta]]*Cos[\[Phi]], -Sin[\[Theta]]*Sin[\[Phi]], -Cos[\[Theta]]}
] // FullSimplify


(* positron average spin *)
With[{chi = \[Chi][{\[Theta], \[Phi]}, +1]},
ToBasis[Outer[Times, chi, Conjugate[chi]]]
== (1/2) {1, -Sin[\[Theta]]*Cos[\[Phi]], -Sin[\[Theta]]*Sin[\[Phi]], -Cos[\[Theta]]}
] // FullSimplify
With[{chi = \[Chi][{\[Theta], \[Phi]}, -1]},
ToBasis[Outer[Times, chi, Conjugate[chi]]]
== (1/2) {1, Sin[\[Theta]]*Cos[\[Phi]], Sin[\[Theta]]*Sin[\[Phi]], Cos[\[Theta]]}
] // FullSimplify
