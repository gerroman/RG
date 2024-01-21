(* ::Package:: *)


BeginPackage["RG`Miscellaneous`"];


stringsortp::usage = "
  stringsortp[expr1, expr2] predicate function for alphabetic sort of strings
"



Begin["`Private`"];


stringsortp[expr1_String, expr2_String] := Module[{
    ch1 = ToCharacterCode[expr1],
	  ch2 = ToCharacterCode[expr2],
		i
  },
	For[i = 1, i <= Min[Length[ch1], Length[ch2]], i++,
	  If[ch1[[i]] > ch2[[i]], Return[False]];
		If[ch1[[i]] < ch2[[i]], Return[True]];
	];
	Return[i + 1 ==  Length[ch1]];
];


Echo[$Context];


EndPackage[];
