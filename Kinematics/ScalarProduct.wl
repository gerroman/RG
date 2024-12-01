BeginPackage["RG`Kinematics`ScalarProduct`"]


ScalarProduct::usage="ScalarProduct[a, b] \[LongDash] general scalar product of two vectors a, b\nScalarProduct[b, a] == ScalarProduct[a, b]\nScalarProduct[a] \[LongDash] expands to ScalarProduct[a, a]"


Begin["`Private`"];


SetAttributes[ScalarProduct, Orderless];


ScalarProduct/:Format[ScalarProduct[a_, a_], TraditionalForm] := DisplayForm[SuperscriptBox[RowBox[{"(", ToBoxes[a, TraditionalForm], ")"}], 2]];
ScalarProduct/:Format[ScalarProduct[a_, b_], TraditionalForm] := DisplayForm[RowBox[{"(", ToBoxes[a, TraditionalForm], "\[CenterDot]", ToBoxes[b, TraditionalForm], ")"}]];
ScalarProduct/:Format[ScalarProduct[a_], TraditionalForm] := DisplayForm[SuperscriptBox[RowBox[{"(", ToBoxes[a,TraditionalForm], ")"}], 2]];


ScalarProduct[a_] := ScalarProduct[a, a];


End[]


EndPackage[]


Print[ToString@StringForm["[info]: `` loaded", $InputFileName]];
