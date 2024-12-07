BeginPackage["RG`Kinematics`FourVector`", {"RG`Kinematics`LorentzIndex`"}]


FourVector::usage="FourVector[p] \[LongDash] 4-vector p\nFourVector[p, \[Mu]] \[LongDash] 4-vector p with Lorentz index \[Mu]"


Begin["`Private`"];


FourVector/:Format[FourVector[a_Symbol, b_LorentzIndex], TraditionalForm] := DisplayForm[
  SuperscriptBox[ToBoxes[a, TraditionalForm], ToBoxes[b, TraditionalForm]]
];
FourVector/:Format[FourVector[a_, b_LorentzIndex], TraditionalForm] := DisplayForm[
  SuperscriptBox[RowBox[{"(", ToBoxes[a, TraditionalForm], ")"}], ToBoxes[b, TraditionalForm]]
];
FourVector/:Format[FourVector[a_], TraditionalForm] := DisplayForm[ToBoxes[a,TraditionalForm]];


End[]


EndPackage[]


fileStamp[]
