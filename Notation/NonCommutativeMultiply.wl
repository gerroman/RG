BeginPackage["RG`Notation`NonCommutativeMultiply`"]


RG`Notation`ncm::usage="ncm \[LongDash] shortcut for NonCommutativeMultiply"


Begin["`Private`"]


preform[expr_Plus] := RowBox[{"(", ToBoxes[expr,TraditionalForm], ")"}];
preform[expr_RG`Notation`sum] := RowBox[{"(", ToBoxes[expr,TraditionalForm], ")"}];
preform[expr_HoldForm] := RowBox[{"(", ToBoxes[expr, TraditionalForm], ")"}];
preform[expr_] := ToBoxes[expr, TraditionalForm];


Unprotect[System`NonCommutativeMultiply]
System`NonCommutativeMultiply /: Format[System`NonCommutativeMultiply[a__], TraditionalForm] := 
  DisplayForm[RowBox[preform /@ {a}]]
Unprotect[System`NonCommutativeMultiply]


RG`Notation`ncm=System`NonCommutativeMultiply


End[]


EndPackage[]
