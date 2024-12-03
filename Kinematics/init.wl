BeginPackage["RG`Kinematics`"]


(*Shortcuts*)
fv=RG`Kinematics`FourVector`FourVector;
sp=RG`Kinematics`ScalarProduct`ScalarProduct;
li=RG`Kinematics`LorentzIndex`LorentzIndex;


Begin["`Private`"]


RG`Kinematics`FourVector`FourVector[p_, mu_Symbol] := RG`Kinematics`FourVector`FourVector[p, RG`Kinematics`LorentzIndex`LorentzIndex[mu]];


End[]


EndPackage[]


Scan[Needs, {
  "RG`Kinematics`LorentzIndex`",
  "RG`Kinematics`FourVector`",
  "RG`Kinematics`ScalarProduct`",
  "RG`Kinematics`ScalarInvariants`",
  "RG`Kinematics`ConservationRules`",
  "RG`Kinematics`ScalarProductRules`",
  "RG`Kinematics`PhysicalRegion`"
}]


Print[ToString@StringForm["[info]: `` loaded", $InputFileName]];
