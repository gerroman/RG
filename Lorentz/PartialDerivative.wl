BeginPackage["RG`Lorentz`PartialDerivative`", {"RG`Lorentz`LorentzIndex`"}]


PartialDerivative::usage = "PartialDerivative[phi] \[LongDash] means derivatives w.r.t. all coordinates,\nPartialDerivative[phi, mu] \[LongDash] means partial derivative D[phi, x[mu]]"


Begin["`Private`"]


Format[PartialDerivative[mu_LorentzIndex], TraditionalForm] :=
  DisplayForm[SubscriptBox[
      ToBoxes["\[PartialD]", TraditionalForm],
      ToBoxes[mu, TraditionalForm]
    ]];

Format[PartialDerivative[phi_Symbol, mu_LorentzIndex][x_],
  TraditionalForm] := DisplayForm[RowBox[{
      ToBoxes[PartialDerivative[mu], TraditionalForm],
      ToBoxes[phi[x], TraditionalForm]
    }]]

Format[PartialDerivative[phi_Symbol, mu_LorentzIndex], TraditionalForm] :=
 DisplayForm[RowBox[{
      ToBoxes[PartialDerivative[mu], TraditionalForm],
      ToBoxes[phi, TraditionalForm]
    }]]


Format[PartialDerivative[phi_, mu_LorentzIndex], TraditionalForm] :=
 DisplayForm[RowBox[{
      ToBoxes[PartialDerivative[mu], TraditionalForm],
      RowBox[{"(", ToBoxes[phi, TraditionalForm], ")"}]
    }]]


Format[PartialDerivative[phi_][x_], TraditionalForm] :=
 DisplayForm[RowBox[{
      ToBoxes["\[PartialD]", TraditionalForm],
      ToBoxes[phi[x], TraditionalForm]
    }]]

Format[PartialDerivative[phi_Symbol], TraditionalForm] := DisplayForm[RowBox[{
      ToBoxes["\[PartialD]", TraditionalForm],
      ToBoxes[phi, TraditionalForm]
    }]]


Format[PartialDerivative[phi_], TraditionalForm] := DisplayForm[RowBox[{
      ToBoxes["\[PartialD]", TraditionalForm],
      RowBox[{"(", ToBoxes[phi, TraditionalForm], ")"}]
    }]]


End[]


EndPackage[]


RG`Scripts`fileStamp[];
