(* ::Package:: *)
(* Functions to perform routine transformations *)

BeginPackage["RG`Calculation`"]


UnderBar::usage = "
  UnderBar[func][args] works as  func[expr, args]
";


Begin["`Private`"]


UnderBar[func_Symbol] := Function[expr, func[expr, ##]] &;


End[]


EndPackage[]