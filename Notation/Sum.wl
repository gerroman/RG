BeginPackage["RG`Notation`Sum`", {"RG`Notation`Force`"}]


sum::usage="sum[expr, var]"


Begin["`Private`"]


sum/:Format[sum[args__], TraditionalForm] := HoldForm[Sum[args]]
sum/:force[sum, opts:OptionsPattern[]] := ReplaceAll[#, sum -> (Sum[##, opts]&)]&


End[]


EndPackage[]


RG`Scripts`fileStamp[]
