BeginPackage["RG`Lorentz`MetricTensor`", {"RG`Lorentz`LorentzIndex`"}]


MetricTensor::usage="MetricTensor[\[Mu]_LorentzIndex, \[Nu]_LorentzIndex] \[LongDash] metric tensor with Lorentz indices"


Begin["`Private`"];


SetAttributes[MetricTensor, Orderless];


Block[{Global`g},
MetricTensor/:Format[MetricTensor[a_LorentzIndex, b_LorentzIndex], TraditionalForm] := DisplayForm[
  SuperscriptBox[ToBoxes[HoldForm[Global`g], TraditionalForm], RowBox[{ToBoxes[a, TraditionalForm], ToBoxes[b, TraditionalForm]}]]
];
]


End[]


EndPackage[]


fileStamp[]
