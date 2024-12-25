BeginPackage["RG`Notation`NonCommutativeMultiply`"]


ncm::usage="ncm = NonCommutativeMultiply"


Begin["`Private`"]


Unprotect[System`NonCommutativeMultiply]
System`NonCommutativeMultiply /: Format[System`NonCommutativeMultiply[a__], TraditionalForm] := DisplayForm[RowBox[ToBoxes[#,TraditionalForm]&/@ {a}]]
Unprotect[System`NonCommutativeMultiply]


ncm=System`NonCommutativeMultiply


End[]


EndPackage[]
