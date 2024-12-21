BeginPackage["RG`Notation`Limit`", "RG`Notation`Force`"]


limit::usage="limit[expr] represents Limit[]"


Begin["`Private`"]


limit/:Format[limit[expr__], TraditionalForm] := HoldForm[Limit[expr]]


limit/:force[limit, opts:OptionsPattern[]] := (
  ReplaceAll[#, limit -> (Limit[##, opts]&)]&
)



End[]


EndPackage[]


RG`Scripts`fileStamp[]
