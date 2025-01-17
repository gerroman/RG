BeginPackage["RG`Notation`i0`"]


Global`i0::usage = "small imaginary quantity"


Begin["`Private`"]


Format[Global`i0, TraditionalForm] := DisplayForm[RowBox[{"\[ImaginaryI]","0"}]];


Unprotect[Plus];
Format[Plus[Global`i0, a_], TraditionalForm] := DisplayForm[RowBox[{
  ToBoxes[a, TraditionalForm],
  "+",
  "\[ImaginaryI]0"
}]];
Format[Plus[(-1)*Global`i0, a_], TraditionalForm] := DisplayForm[RowBox[{
  ToBoxes[a, TraditionalForm],
  "-",
  "\[ImaginaryI]0"
}]];
Protect[Plus];


End[]


EndPackage[]
