(* ::Package:: *)

BeginPackage["RG`Lorentz`FourVector`", {"RG`Lorentz`LorentzIndex`"}]


FourVector::usage="FourVector[p] \[LongDash] 4-vector p\nFourVector[p, \[Mu]] \[LongDash] 4-vector p with Lorentz index \[Mu]"


Begin["`Private`"];


Format[FourVector[a_Symbol, b_], TraditionalForm] := DisplayForm[
  SuperscriptBox[ToBoxes[a, TraditionalForm], ToBoxes[b, TraditionalForm]]
];
Format[FourVector[a_, b_LorentzIndex], TraditionalForm] := DisplayForm[
  SuperscriptBox[RowBox[{"(", ToBoxes[a, TraditionalForm], ")"}], ToBoxes[b, TraditionalForm]]
];
Format[FourVector[a_, b_Integer], TraditionalForm] := DisplayForm[
  SuperscriptBox[ToBoxes[a, TraditionalForm], b]
];
Format[FourVector[a_], TraditionalForm] := DisplayForm[ToBoxes[a,TraditionalForm]];


End[]


EndPackage[]


RG`Scripts`fileStamp[]
