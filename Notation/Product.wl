BeginPackage["RG`Notation`Product`", {"RG`Notation`Force`"}]


product::usage="product[expr, var]"


Begin["`Private`"]


product/:Format[product[args__], TraditionalForm] := HoldForm[Product[args]]
product/:force[product, opts:OptionsPattern[]] := ReplaceAll[#, product -> Product[##, opts]&]&
product/:Format[product[expr__, {}], TraditionalForm] := DisplayForm[RowBox[
	ToBoxes[#, TraditionalForm]&/@{expr}
]]

End[]


EndPackage[]
  

RG`Scripts`fileStamp[]
