BeginPackage["RG`Lorentz`PartialDerivative`"]


PartialDerivative::usage = "PartialDerivative[phi] \[LongDash] means derivatives w.r.t. all coordinates,\nPartialDerivative[phi, mu] \[LongDash] means partial derivative D[phi, x[mu]]"


Begin["`Private`"]


Format[PartialDerivative[phi_Symbol, mu_], TraditionalForm] := DisplayForm[
  RowBox[{
    SubscriptBox["\[PartialD]", mu],
    ToBoxes[phi, TraditionalForm]
  }]
]
Format[PartialDerivative[phi_Symbol, {mu_, i_}], TraditionalForm] := DisplayForm[
  RowBox[{
    SubsuperscriptBox["\[PartialD]", mu, i],
    ToBoxes[phi, TraditionalForm]
  }]
]


Format[PartialDerivative[phi_, {mu_,i_}], TraditionalForm] := DisplayForm[
  RowBox[{
    SubsuperscriptBox["\[PartialD]", mu, i],
    RowBox[{"(", ToBoxes[phi, TraditionalForm], ")"}]
  }]
]
Format[PartialDerivative[phi_, mu_], TraditionalForm] := DisplayForm[
  RowBox[{
    SubscriptBox["\[PartialD]", mu],
    RowBox[{"(", ToBoxes[phi, TraditionalForm], ")"}]
  }]
]


Format[PartialDerivative[phi_Symbol], TraditionalForm] := DisplayForm[
  RowBox[{
    ToBoxes["\[PartialD]", TraditionalForm],
    ToBoxes[phi, TraditionalForm]
  }]
]


Format[PartialDerivative[phi_], TraditionalForm] := DisplayForm[
  RowBox[{
    ToBoxes["\[PartialD]", TraditionalForm],
    RowBox[{"(", ToBoxes[phi, TraditionalForm], ")"}]
  }]
]


End[]


EndPackage[]


RG`Scripts`fileStamp[];
