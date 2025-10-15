(* ::Package:: *)

(* @file StandardGammaMatrices.m *)
(* @brief Standard representation of Dirac and Pauli matrices, decomposition to standard bases, standard spinors *)


BeginPackage["RG`QED`StandardGammaMatrices`"]


DiracId::usage = "DiracIdentity matrix 4x4"
DiracZero::usage = "DiracZero matrix 4x4"
DiracGammaMatrix::usage = "DiracGammaMatrix - Dirac matrices in the standard representation"
DiracSigmaMatrix::usage = "DiracSigmaMatrix[mu, nu] = (I/2)[DiracGammaMatrix[mu], DiracGammaMatrix[nu]]"
DiracBasis::usage = "DiracBasis in the space 4x4"
DiracIdxLoweringCoeffs::usage = "taking into accout metric rising/lowering coefficients"


PauliId::usage = "DiracIdentity matrix 2x2"
PauliZero::usage = "DiracZero matrix 2x2"
PauliSigmaMatrix::usage = "Pauli DiracSigmaMatrix-matrices 2x2"
PauliBasis::usage "basis in the space 2x2"


ToBasis::usage = "decompose matrix to basis 2x2 or 4x4"


PhiSpinor::usage = "PhiSpinor[{\[Theta], \[Phi]}, \[Sigma]:(+1|-1)] spinor with definite spin projection \[Sigma] on the direction defined by {\[Theta], \[Phi]}"
ChiSpinor::usage = "ChiSpinor[{\[Theta], \[Phi]}, \[Sigma]:(+1|-1)] conjugated spinor with definite spin projection \[Sigma] on the direction defined by {\[Theta], \[Phi]}"


USpinor::usage = "USpinor[p, m, \[CurlyPhi]] is Dirac bispinor u of the particle with 4-momentum p, mass m, and spin state in the rest frame defined by spinor \[CurlyPhi]"
VSpinor::usage = "VSpinor[p, m, \[Chi]] is Dirac bispinor v of the particle with 4-momentum p, mass m, and spin state in the rest frame defined by spinor \[Chi]"


PauliUCharge::usage = "PauliUCharge \[LongDash] charge conjugation matrix for Pauli spinors"


DiracUCharge::usage = "DiracUCharge \[LongDash] charge conjugation matrix for Dirac spinors"
DiracUParity::usage = "DiracUParity \[LongDash] parity conjugation matrix for Dirac spinors"
DiracUTime::usage = "DiracUTime \[LongDash] time conjugation matrix for Dirac spinors"


Begin["`Private`"]


PauliId = IdentityMatrix[2];
PauliZero = ConstantArray[0, {2, 2}];
PauliSigmaMatrix[i:(1|2|3)] := PauliMatrix[i];

PauliBasis = {PauliId, PauliSigmaMatrix[1], PauliSigmaMatrix[2], PauliSigmaMatrix[3]};

DiracId = ArrayFlatten[{{PauliId, PauliZero},{PauliZero, PauliId}}];
DiracZero = ArrayFlatten[{{PauliZero, PauliZero},{PauliZero, PauliZero}}];


DiracGammaMatrix[0] = ArrayFlatten[{{PauliId, PauliZero}, {PauliZero, -PauliId}}];
DiracGammaMatrix[i:(1|2|3)] := ArrayFlatten[{{PauliZero, PauliSigmaMatrix[i]},{-PauliSigmaMatrix[i], 0}}];


DiracGammaMatrix[{}, 0] = DiracGammaMatrix[0];
DiracGammaMatrix[{}, i:(1|2|3)] := (-1)*DiracGammaMatrix[i];


DiracGammaMatrix[5] = (+I) * DiracGammaMatrix[0].DiracGammaMatrix[1].DiracGammaMatrix[2].DiracGammaMatrix[3];


DiracSigmaMatrix[mu:(0|1|2|3), nu:(0|1|2|3)] := (+I/2)*(DiracGammaMatrix[mu].DiracGammaMatrix[nu] - DiracGammaMatrix[nu].DiracGammaMatrix[mu]);


DiracBasis = {
  DiracId,
  DiracGammaMatrix[5],
  Sequence@@Array[DiracGammaMatrix, 4, 0],
  Sequence@@Array[I*DiracGammaMatrix[#].DiracGammaMatrix[5]&, 4, 0],
  Sequence@@Flatten[Table[DiracSigmaMatrix[mu, nu], {mu, 0, 2}, {nu, mu + 1, 3}], 1]
};


DiracIdxLoweringCoeffs = {
+1,
+1,
+1, -1, -1, -1,
+1, -1, -1, -1,
-1, -1, -1, +1, +1, +1
};


ToBasis[m_/;Dimensions[m] == {4, 4}] := (1 / 4) * Map[Tr[m.#]&, DiracBasis] * DiracIdxLoweringCoeffs;
ToBasis[m_/;Dimensions[m] == {2, 2}] := (1 / 2) * Map[Tr[m.#]&, PauliBasis];


PhiSpinor[{theta_, phi_}, +1] := {Cos[theta/2], Exp[I phi] Sin[theta/2]};
PhiSpinor[{theta_, phi_}, -1] := {-Exp[-I phi] Sin[theta/2], Cos[theta/2]};


ChiSpinor[{theta_, phi_}, +1] := {-Exp[-I phi] Sin[theta/2], Cos[theta/2]};
ChiSpinor[{theta_, phi_}, -1] := {-Cos[theta/2], -Exp[I phi] Sin[theta/2]};


PauliUCharge = (-I) * PauliMatrix[2];


USpinor[p4_List, m_, phi_List] := With[{epsilon = First[p4], p = Rest[p4], sigma = Array[PauliSigmaMatrix, 3]},
Sqrt[epsilon + m] * Flatten[{phi, Total[sigma * p].phi / (epsilon + m)}]
]


VSpinor[p4_List, m_, chi_List] := With[{epsilon = First[p4], p = Rest[p4], sigma = Array[PauliSigmaMatrix, 3]},
Sqrt[epsilon + m] * Flatten[{Total[sigma * p].chi / (epsilon + m), chi}]
]


DiracUCharge = (+I) * DiracGammaMatrix[2]
DiracUParity = DiracGammaMatrix[0]
DiracUTime = DiracGammaMatrix[1].DiracGammaMatrix[3]


End[]


EndPackage[]
