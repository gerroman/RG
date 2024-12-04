BeginPackage["RG`Kinematics`"]


(*Shortcuts*)
fv=RG`Kinematics`FourVector`FourVector;
sp=RG`Kinematics`ScalarProduct`ScalarProduct;
li=RG`Kinematics`LorentzIndex`LorentzIndex;
mt=RG`Kinematics`MetricTensor`MetricTensor;

Begin["`Private`"]


RG`Kinematics`FourVector`FourVector[p_, mu_Symbol] := RG`Kinematics`FourVector`FourVector[p, RG`Kinematics`LorentzIndex`LorentzIndex[mu]];
RG`Kinematics`MetricTensor`MetricTensor[mu_Symbol, nu_Symbol] := RG`Kinematics`MetricTensor`MetricTensor[RG`Kinematics`LorentzIndex`LorentzIndex[mu], RG`Kinematics`LorentzIndex`LorentzIndex[nu]]


End[]


EndPackage[]


Scan[Needs, {
  "RG`Kinematics`LorentzIndex`",
  "RG`Kinematics`FourVector`",
  "RG`Kinematics`ScalarProduct`",
  "RG`Kinematics`ScalarInvariants`",
  "RG`Kinematics`ConservationRules`",
  "RG`Kinematics`ScalarProductRules`",
  "RG`Kinematics`PhysicalRegion`",
  "RG`Kinematics`MetricTensor`"
}]


Print[ToString@StringForm["[info]: `` loaded", $InputFileName]];
