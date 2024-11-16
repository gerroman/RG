(* ::Package:: *)


BeginPackage["RG`GiNaC`", {"RG`Integrate`"}]


Global`G::usage = "G[{z1, ..., zn}, y] represent Goncharov interative polylogarithm";


EvalG::usage = "EvalG[{Re[z1], ..., Re[zn]}, {Im[z1], .., Im[zn]}, y] evaluate iterative polylog for Complex {`z1`, ..., `zn`} and Real `y`";


GoncharovG::usage = "GoncharovG[{z1, ...}, y] get explicit form of Goncharov polylogarithms in terms of folded integrals";


GeneralizedGoncharovG::usage = "GeneralizedGoncharovG[{m1, ...}][{z1, ...}, y] get explicit form of generalized Goncharov polylogarithms";


Begin["`Private`"];


Install[FileNameJoin[{"RG", "GiNaC", "bin", "G.exe"}]];


Global`G /: N[Global`G[zs_List, y_]] := Complex @@ EvalG[N@Re[zs], N@Im[zs], N@y];


Format[GoncharovG[zs_List, y_], TraditionalForm] := DisplayForm@RowBox[{Global`G, "(", Row[zs, ","], ";", y, ")"}];


Format[GeneralizedGoncharovG[ms__][zs_List, y_], TraditionalForm] :=
  DisplayForm@RowBox[{Subscript[Global`G, Row[{ms}, ","]], "(", Row[zs, ","], ";", y, ")"}];


Format[GeneralizedGoncharovG[ms_List][zs_List, y_], TraditionalForm] :=
  DisplayForm@RowBox[{Subscript[Global`G, Row[ms, ","]], "(", Row[zs, ","], ";", y, ")"}];


GoncharovG[{}, y_] := 1;


GoncharovG[(zs_List /; Not@MemberQ[zs, 0]), y_] := (
  With[{n = Length[zs]},
    With[{ts = Table[Unique["tau$"], n]},
      integrate[
        Inner[1/(#1 - #2) &, ts, zs, Times],
  	    Sequence @@ Transpose[{ts, ConstantArray[0, n], Most[ts] // Prepend[y]}]
      ]
    ]
  ]
);


GoncharovG[zs : {0 ..}, y_] := With[{r = Length[zs]}, 1/r! Log[y]^r]


GoncharovG[{z_ /; z =!= 0, zs___}, y_] := (
  With[{ty = Unique["tau$"]},
    integrate[GoncharovG[{zs}, ty] / (ty - z), {ty, 0, y}]
  ]
);


GeneralizedGoncharovG::args = "unequal lengths of indices lists";
GeneralizedGoncharovG[ms_List][zs_List, y_] := If[Length[ms] == Length[zs], 
  With[{
      idxs=Flatten@Riffle[ConstantArray[0, #] & /@ (ms - 1), zs]
    },
    GoncharovG[idxs, y]
  ],
  Message[GeneralizedGoncharovG::args];
  Hold[GeneralizedGoncharovG[ms][zs, y]]
];


GeneralizedGoncharovG[ms__][zs_List, y_] := GeneralizedGoncharovG[{ms}][zs, y];


End[]


EndPackage[]


Print[ToString@StringForm["[info]: '``' loaded", FileNameTake[$InputFileName, -3]]];
