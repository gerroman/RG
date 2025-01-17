(* ::Package:: *)

BeginPackage["RG`GiNaC`", {"RG`Notation`Integrate`"}]


Global`G::usage = "G[{z1, ..., zn}, y] represent Goncharov interative polylogarithm";


EvalG::usage = "EvalG[{Re[z1], ..., Re[zn]}, {Im[z1], .., Im[zn]}, y] evaluate iterative polylog for Complex {`z1`, ..., `zn`} and Real `y`";


Global`H::usage = "H[{m1, ..., mn}, y] harmonic polylogarithm";


EvalH::usage = "EvalH[{m1, ..., mn}, x] evaluate harmonic polylogarithm for integer {m1, ..., mn} and Real `x`";


GoncharovG::usage = "GoncharovG[{z1, ...}, y] get explicit form of Goncharov polylogarithms in terms of folded integrals";


GeneralizedGoncharovG::usage = "GeneralizedGoncharovG[{m1, ...}][{z1, ...}, y] get explicit form of generalized Goncharov polylogarithms";


Begin["`Private`"];


EvalG::nofile="can not find G.exe to evaluate Goncharov polylogarithms numerically"
With[{fname=FileNameJoin[{"RG", "GiNaC", "bin", "G.exe"}]},
If[FindFile[fname] =!= $Failed, (
    Print[fname];
    Install[fname];
    Global`G /: N[Global`G[zs_List, y_]] := Complex @@ EvalG[N@Re[zs], N@Im[zs], N@y];
    Global`H /: N[Global`H[ms_List, y_]] := Complex @@ EvalH[ms, N@y];
  )
  ,
  Message[EvalG::nofile]
]]


Format[GoncharovG[zs_List, y_], TraditionalForm] := DisplayForm@RowBox[{Global`G, "(", Row[zs, ","], ";", y, ")"}];


Format[GeneralizedGoncharovG[ms__][zs_List, y_], TraditionalForm] :=
  DisplayForm@RowBox[{Subscript[Global`G, Row[{ms}, ","]], "(", Row[zs, ","], ";", y, ")"}];


Format[GeneralizedGoncharovG[ms_List][zs_List, y_], TraditionalForm] :=
  DisplayForm@RowBox[{Subscript[Global`G, Row[ms, ","]], "(", Row[zs, ","], ";", y, ")"}];


GoncharovG[{}, y_] := 1;


GoncharovG[(zs_List), y_] := (
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


(*Based on rule derived by R.N.Lee*)
Global`G /: Series[Global`G[idxs : {as___, a_}, z_], {z_, 0, order_}] := (
  With[{l = Length[idxs]},
   Fold[
     Integrate[#1/(z - #2), z] &,
     Integrate[Series[1/(z - a), {z, 0, Max[order - l, 0]}], z],
     Reverse[{as}]
     ] // MapAt[Together, #, {3, All}] &
   ]
) /; FreeQ[idxs, 0, 1];


End[]


EndPackage[]


Print[ToString@StringForm["[info]: '``' loaded", FileNameTake[$InputFileName, -3]]];
