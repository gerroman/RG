BeginPackage["RG`Kinematics`", {
  "RG`Kinematics`LorentzIndex`",
  "RG`Kinematics`FourVector`",
  "RG`Kinematics`ScalarProduct`",
  "RG`Kinematics`ScalarInvariants`",
  "RG`Kinematics`ConservationRules`",
  "RG`Kinematics`ScalarProductRules`",
  "RG`Kinematics`PhysicalRegion`"
}]


(*Shortcuts*)
fv=FourVector;
sp=ScalarProduct;
li=LorentzIndex;


Begin["`Private`"]


FourVector[p_, mu_Symbol] := FourVector[p, LorentzIndex[mu]];


End[]


EndPackage[]


Print[ToString@StringForm["[info]: `` loaded", $InputFileName]];
