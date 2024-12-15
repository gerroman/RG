BeginPackage["RG`FeynmanParameters`", {"RG`Integrate`"}]

GetFeynmanParametrization::usage="GetParametrization[l, {m2 - sp[l+_], \[Ellipsis]}]"

Begin["`Private`"]

patternDenominator[l_]:=((_.-_[(_:l)+_.])|(_.-_[(_:l) + a_.,(_:l) + a_.]))

SetAttributes[GetFeynmanParametrization, Listable];
GetFeynmanParametrization[l_, denominators_List, dim_] := Module[
  {},
  Null
];

End[]


EndPackage[]

RG`Scripts`filestamp[]