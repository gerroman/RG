(* ::Package:: *)

BeginPackage["RG`Lorentz`DiracMatrix`", {"RG`Lorentz`LorentzIndex`","RG`Lorentz`FourVector`"}]


DiracMatrix::usage="DiracMatrix[\[Mu]] \[LongDash] Dirac matrix with Lorentz index \[Mu]"
DiracSlash::usage="DiracSlash[l] \[LongDash] Dirac matrix convolution with 4-vector l"
DiracId::usage="DiracId \[LongDash] identity matrix"


Begin["`Private`"];


DiracMatrix/:Format[DiracMatrix[l_LorentzIndex], TraditionalForm] :=
 DisplayForm[SuperscriptBox["\[Gamma]", ToBoxes[l, TraditionalForm]]]


DiracMatrix/:Format[DiracMatrix[i_Integer], TraditionalForm] := DisplayForm[SuperscriptBox["\[Gamma]", i]]


DiracId/:Format[DiracId, TraditionalForm] := DisplayForm[ToBoxes[HoldForm[Global`\[DoubleStruckOne]]]];

Unprotect[Times]
Format[Times[a_, DiracId], TraditionalForm] := DisplayForm[ToBoxes[a, TraditionalForm]];
Protect[Times]


DiracSlash/:Format[DiracSlash[l_FourVector], TraditionalForm] :=
 DisplayForm[ToBoxes[OverHat[l], TraditionalForm]]


DiracSlash/:Format[DiracSlash[l_Plus], TraditionalForm] := DisplayForm[
  ToBoxes[
    OverHat[l] //
      ReplaceRepeated[#, expr:_OverHat:> Distribute[expr]]& //
      ReplaceRepeated[#, OverHat[v_ factor_/;(NumberQ[factor]||NumericQ[factor])] :> factor OverHat[v]]&
    ,
    TraditionalForm
  ]
]


End[]


EndPackage[]


RG`Scripts`fileStamp[]
