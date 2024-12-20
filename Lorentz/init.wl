(* ::Package:: *)

Scan[Needs, {
  "RG`Lorentz`LorentzIndex`",
  "RG`Lorentz`FourVector`",
  "RG`Lorentz`ScalarProduct`",
  "RG`Lorentz`MetricTensor`",
  "RG`Lorentz`DiracMatrix`",
  "RG`Lorentz`PartialDerivative`",
  "RG`Lorentz`LorentzTensor`"
}];


BeginPackage["RG`Lorentz`", {
  "RG`Lorentz`LorentzIndex`",
  "RG`Lorentz`FourVector`",
  "RG`Lorentz`ScalarProduct`",
  "RG`Lorentz`MetricTensor`",
  "RG`Lorentz`DiracMatrix`",
  "RG`Lorentz`PartialDerivative`",
  "RG`Lorentz`LorentzTensor`"
}]


Begin["`Private`"]


(*Shortcuts*)
FourVector[p_Symbol, mu_Symbol] := FourVector[FourVector[p], LorentzIndex[mu]];
MetricTensor[mu_Symbol, nu_Symbol] := MetricTensor[LorentzIndex[mu], LorentzIndex[nu]]
DiracMatrix[mu_Symbol] := DiracMatrix[LorentzIndex[mu]];
DiracSlash[p_Symbol] := DiracSlash[FourVector[p]];


End[]


EndPackage[]


RG`Scripts`fileStamp[];
