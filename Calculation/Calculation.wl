(* ::Package:: *)

(* Functions to perform routine transformations *)

BeginPackage["RG`Calculation`"]


OverTilde::usage = "
  OverTilde[func][args] works as  func[expr, args]
";


Begin["`Private`"]


OverTilde[func_Symbol] := Function[expr, func[expr, ##]] &;


End[]


EndPackage[]
