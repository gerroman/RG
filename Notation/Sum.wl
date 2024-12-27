BeginPackage["RG`Notation`Sum`", {"RG`Notation`Force`"}]


sum::usage="sum[expr, var]"


Begin["`Private`"]


sum/:Format[sum[expr_, var_], TraditionalForm] := DisplayForm[RowBox[{
  UnderscriptBox[ToBoxes[Style["\[Sum]",Larger], TraditionalForm], ToBoxes[var,TraditionalForm]]
  ,
  ToBoxes[expr, TraditionalForm]
}]];
sum/:Format[sum[expr_, {var_, i_, j_}], TraditionalForm] := DisplayForm[RowBox[{
  UnderoverscriptBox[ToBoxes["\[Sum]", TraditionalForm], ToBoxes[var == i, TraditionalForm], ToBoxes[j,TraditionalForm]]
  ,
  ToBoxes[expr, TraditionalForm]
}]];

sum/:force[sum, opts:OptionsPattern[]] := ReplaceAll[#, sum -> (Sum[##, opts]&)]&;


End[]


EndPackage[]


RG`Scripts`fileStamp[]
