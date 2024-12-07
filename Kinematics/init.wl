(* ::Package:: *)

Scan[Needs, {
  "RG`Kinematics`LorentzIndex`",
  "RG`Kinematics`FourVector`",
  "RG`Kinematics`ScalarProduct`",
  "RG`Kinematics`ScalarInvariants`",
  "RG`Kinematics`ConservationRules`",
  "RG`Kinematics`ScalarProductRules`",
  "RG`Kinematics`PhysicalRegion`",
  "RG`Kinematics`MetricTensor`"
}];


BeginPackage["RG`Kinematics`", {
  "RG`Kinematics`LorentzIndex`",
  "RG`Kinematics`FourVector`",
  "RG`Kinematics`MetricTensor`"
}]


(*Shortcuts*)
Begin["`Private`"]


FourVector[p_, mu_Symbol] := FourVector[p, LorentzIndex[mu]];
MetricTensor[mu_Symbol, nu_Symbol] := MetricTensor[LorentzIndex[mu], LorentzIndex[nu]]


End[]


EndPackage[]


fileStamp[]
