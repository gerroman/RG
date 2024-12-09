(* ::Package:: *)

Scan[Needs, {
  "RG`Lorentz`LorentzIndex`",
  "RG`Lorentz`FourVector`",
  "RG`Lorentz`ScalarProduct`",
  "RG`Lorentz`MetricTensor`",
  "RG`Kinematics`ScalarInvariants`",
  "RG`Kinematics`ConservationRules`",
  "RG`Kinematics`ScalarProductRules`",
  "RG`Kinematics`PhysicalRegion`"
}];


BeginPackage["RG`Kinematics`", {
  "RG`Lorentz`LorentzIndex`",
  "RG`Lorentz`FourVector`",
  "RG`Lorentz`MetricTensor`"
}]


(*Shortcuts*)
Begin["`Private`"]


FourVector[p_, mu_Symbol] := FourVector[p, LorentzIndex[mu]];
MetricTensor[mu_Symbol, nu_Symbol] := MetricTensor[LorentzIndex[mu], LorentzIndex[nu]]


End[]


EndPackage[]


RG`Scripts`fileStamp[];
