BeginPackage["RG`Kinematics`LorentzIndex`"]


LorentzIndex::usage="LorentzIndex[\[Mu]] \[LongDash] Lorentz index \[Mu]\nLorentzIndex[\[Mu], i] \[LongDash] Lorentz index \[Mu] with subscript i"


Begin["`Private`"];


LorentzIndex/:Format[LorentzIndex[a_], TraditionalForm] := DisplayForm[ToBoxes[a,TraditionalForm]];
LorentzIndex/:Format[LorentzIndex[a_, i_], TraditionalForm] := DisplayForm[SubscriptBox[ToBoxes[a,TraditionalForm], ToBoxes[i,TraditionalForm]]];


End[]


EndPackage[]


Print[ToString@StringForm["[info]: `` loaded", $InputFileName]];
