(* ::Package:: *)

BeginPackage["RG`Lorentz`ScalarProduct`"]


ScalarProduct::usage="ScalarProduct[a, b] \[LongDash] general scalar product of two vectors a, b\nScalarProduct[b, a] == ScalarProduct[a, b]\nScalarProduct[a] \[LongDash] expands to ScalarProduct[a, a]"


EvaluateScalarProducts::usage="EvaluateScalarProducts[expr] \[LongDash] evaluates scalar products using distributive and associative laws\nEvaluateScalarProducts[expr, rules] \[LongDash] evalates scalar products using rules"


Begin["`Private`"];


SetAttributes[ScalarProduct, Orderless];


ScalarProduct/:Format[ScalarProduct[a_, a_], TraditionalForm] :=
  DisplayForm[SuperscriptBox[RowBox[{"(", ToBoxes[a, TraditionalForm], ")"}], 2]];

ScalarProduct/:Format[ScalarProduct[a_Plus, b_Plus], TraditionalForm] :=
  DisplayForm[RowBox[{"(", "(", ToBoxes[a, TraditionalForm],")", "\[CenterDot]", "(", ToBoxes[b, TraditionalForm],")", ")"}]];
ScalarProduct/:Format[ScalarProduct[a_, b_Plus], TraditionalForm] :=
  DisplayForm[RowBox[{"(", ToBoxes[a, TraditionalForm], "\[CenterDot]", "(", ToBoxes[b, TraditionalForm],")", ")"}]];
ScalarProduct/:Format[ScalarProduct[a_Plus, b_], TraditionalForm] :=
  DisplayForm[RowBox[{"(", "(", ToBoxes[a, TraditionalForm],")", "\[CenterDot]", ToBoxes[b, TraditionalForm], ")"}]];
ScalarProduct/:Format[ScalarProduct[a_, b_], TraditionalForm] :=
  DisplayForm[RowBox[{"(", ToBoxes[a, TraditionalForm], "\[CenterDot]", ToBoxes[b, TraditionalForm], ")"}]];
ScalarProduct/:Format[ScalarProduct[a_], TraditionalForm] :=
  DisplayForm[SuperscriptBox[RowBox[{"(", ToBoxes[a,TraditionalForm], ")"}], 2]];


ScalarProduct[a_] := ScalarProduct[a, a];


EvaluateScalarProducts[expr_] := expr //
  ReplaceRepeated[#, sp_ScalarProduct :> Distribute[sp]] & //
  ReplaceRepeated[#, ScalarProduct[a___, (factor_/;(NumberQ[factor]||NumberQ[factor])) b_, c___] :> factor ScalarProduct[a, b, c]]&;


EvaluateScalarProducts[expr_, rules_] := expr //
  EvaluateScalarProducts //
  ReplaceAll[rules]


End[]


EndPackage[]


RG`Scripts`fileStamp[];
