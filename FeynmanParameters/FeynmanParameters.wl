(* ::Package:: *)

BeginPackage["RG`FeynmanParameters`", {"RG`Integrate`"}]


GetFeynmanParametrization::usage="GetParametrization[denominators_List]"
RG`FeynmanParameters`x::usage="x[i] \[LongDash] Feynman parameter"


Begin["`Private`"]


RG`FeynmanParameters`x /: Format[RG`FeynmanParameters`x[i_Integer], TraditionalForm] := DisplayForm[SubscriptBox[Style["x",Italic], i]]


GetFeynmanParametrization[denominators_List, powers_List] := Module[
  {
    n = Total[powers],
    xs,
    commonFactor,
    integrand,
    result
  },
  xs = Array[RG`FeynmanParameters`x, n];
  commonFactor = Gamma[n] / Times @@ (Gamma /@ powers);
  integrand = Times@@(xs^(powers - 1)) *
    DiracDelta[1 - Total[xs]] *
    (Total[xs * denominators])^(-n);
  result = Fold[
    integrate[#1, {#2, 0, Infinity}]&,
    integrand,
    xs
  ]
];
GetFeynmanParametrization[denominators_List] := (
  GetFeynmanParametrization[
    denominators,
    ConstantArray[1, Length[denominators]]
  ]
);


End[]


EndPackage[]


RG`Scripts`fileStamp[]
