(* ::Package:: *)

BeginPackage["RG`Lorentz`LorentzTensor`", {"RG`Lorentz`LorentzIndex`"}]


LorentzTensor::usage="LorentzTensor[T] \[LongDash] 4-tensor p\nLorentzTensor[p, \[Mu]..] \[LongDash] 4-tensor p with Lorentz indices \[Mu].."


Begin["`Private`"];


Format[LorentzTensor[a_Symbol, bs_List], TraditionalForm] := DisplayForm[
  SuperscriptBox[ToBoxes[a, TraditionalForm], RowBox[ToBoxes[#, TraditionalForm]&/@bs]]
] /; MatchQ[bs, {(_LorentzIndex|_Integer)..}];

Format[LorentzTensor[a_, bs_List], TraditionalForm] := DisplayForm[
  SuperscriptBox[RowBox[{"(", ToBoxes[a, TraditionalForm], ")"}], RowBox[ToBoxes[#, TraditionalForm]&/@bs]]
] /; MatchQ[bs, {(_LorentzIndex|_Integer)..}];

Format[LorentzTensor[a_], TraditionalForm] := DisplayForm[ToBoxes[a,TraditionalForm]];


End[]


EndPackage[]


RG`Scripts`fileStamp[]
