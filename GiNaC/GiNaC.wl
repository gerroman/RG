(* ::Package:: *)

BeginPackage["RG`GiNaC`", {"RG`Notation`Integrate`"}]


Global`G::usage = "G[{z1, ..., zn}, y] represent Goncharov interative polylogarithm";


EvalG::usage = "EvalG[{Re[z1], ..., Re[zn]}, {Im[z1], .., Im[zn]}, y] evaluate iterative polylog for Complex {`z1`, ..., `zn`} and Real `y`";


Global`H::usage = "H[{m1, ..., mn}, y] harmonic polylogarithm";


EvalH::usage = "EvalH[{m1, ..., mn}, x] evaluate harmonic polylogarithm for integer {m1, ..., mn} and Real `x`";


GoncharovG::usage = "GoncharovG[{z1, ...}, y] get explicit form of Goncharov polylogarithms in terms of folded integrals";


GeneralizedGoncharovG::usage = "GeneralizedGoncharovG[{m1, ...}][{z1, ...}, y] get explicit form of generalized Goncharov polylogarithms";


ExpandGProduct::usage = "ExpandGProduct[G[{i1,\[Ellipsis]},x]*G[{j1,\[Ellipsis]},x]] \[LongDash] expands products of the two Gs using shuffle product of the indices"


ReduceGZeros::usage = "ReduceGZeros[expr] \[LongDash] reduce trailing zeros of all Gs appearing in the expr, convert G[{0..},x] to power of Log[x]"


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


(* ::Section:: *)
(*\:041f\:0440\:0435\:0434\:0441\:0442\:0430\:0432\:043b\:0435\:043d\:0438\:0435*)


Format[GoncharovG[zs_List, y_], TraditionalForm] := DisplayForm@RowBox[{Global`G, "(", Row[zs, ","], ";", y, ")"}];


Format[GeneralizedGoncharovG[ms__][zs_List, y_], TraditionalForm] :=
  DisplayForm@RowBox[{Subscript[Global`G, Row[{ms}, ","]], "(", Row[zs, ","], ";", y, ")"}];


Format[GeneralizedGoncharovG[ms_List][zs_List, y_], TraditionalForm] :=
  DisplayForm@RowBox[{Subscript[Global`G, Row[ms, ","]], "(", Row[zs, ","], ";", y, ")"}];


GoncharovG[{}, y_] := 1;


(*[TODO]: remove ower-definition *)


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


GoncharovG[zs:{0 ..}, y_] := With[{r = Length[zs]}, 1/r! Log[y]^r]


GoncharovG[{z_/; z =!= 0,  zs___}, y_] := (
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


(* ::Section:: *)
(*\:0420\:0430\:0437\:043b\:043e\:0436\:0435\:043d\:0438\:0435 G \:043f\:0440\:0438 \:043e\:0442\:0441\:0443\:0442\:0441\:0442\:0432\:0438\:0438 \:043d\:0443\:043b\:0435\:0432\:044b\:0445 \:0438\:043d\:0434\:0435\:043a\:0441\:043e\:0432*)


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


(* ::Section:: *)
(*\:041f\:0440\:0435\:043e\:0431\:0440\:0430\:0437\:043e\:0432\:0430\:043d\:0438\:0435 \:0444\:0443\:043d\:043a\:0446\:0438\:0438 G \:043f\:0440\:0438 \:043d\:0430\:043b\:0438\:0447\:0438\:0438 \:043d\:0443\:043b\:0435\:0432\:044b\:0445 \:0438\:043d\:0434\:0435\:043a\:0441\:043e\:0432 \:043d\:0430 \:043f\:043e\:0441\:043b\:0435\:0434\:043d\:0438\:0445 \:043c\:0435\:0441\:0442\:0430\:0445*)


(* ::Text:: *)
(*\:0420\:0430\:0441\:043a\:0440\:044b\:0442\:0438\:0435 \:043f\:0440\:043e\:0438\:0437\:0432\:0435\:0434\:0435\:043d\:0438\:044f \:043f\:0430\:0440\:044b G-\:0444\:0443\:043d\:043a\:0446\:0438\:0439 (shuffle product \:0434\:0432\:0443\:0445 \:043d\:0430\:0431\:043e\:0440\:043e\:0432)*)


ExpandGProduct[Global`G[l1_, x_] * Global`G[l2_, x_]] := (
  (*log[{l1, l2}];*)
  With[{n1=Length[l1], n2=Length[l2]},
    With[{idxs = Range[n1+n2]},
	  With[{ifunc = Replace[idxs, Join[Thread[#->l1], Thread[Complement[idxs, #]->l2]], 1]&},
        Total[Global`G[ifunc[#], x]& /@ Subsets[idxs, {n1}]]
      ]
    ]
  ]
)


(* ::Text:: *)
(*\:0418\:0437\:0431\:0430\:0432\:043b\:0435\:043d\:0438\:0435 \:043e\:0442 \:043d\:0443\:043b\:0435\:0432\:044b\:0445 \:0438\:043d\:0434\:0435\:043a\:0441\:043e\:0432 \:043d\:0430 \:043f\:043e\:0441\:043b\:0435\:0434\:043d\:0438\:0445 \:043c\:0435\:0441\:0442\:0430\:0445*)


SetAttributes[ReduceGZeros,Listable]
ReduceGZeros[Global`G[b:{0..}, x_]] := With[{n=Length[b]}, Power[Log[x], n]/n!];
ReduceGZeros[Global`G[{a__, b:0..}, x_]] := (
	log[{a, b}];
	ReduceGZeros[Global`G[{b}, x]] * Global`G[{a}, x] 
	- ReduceGZeros[ExpandGProduct[Global`G[{b}, x] Global`G[{a}, x]] - Global`G[{a, b}, x]]
);
ReduceGZeros[a_ + b_] := ReduceGZeros[a] + ReduceGZeros[b];
ReduceGZeros[a_ * b_] := ReduceGZeros[a] * ReduceGZeros[b];
ReduceGZeros[Power[a_, b_]] := ReduceGZeros[a]^b;
ReduceGZeros[a_] := a


(* ::Section:: *)
(*\:0420\:0430\:0437\:043b\:043e\:0436\:0435\:043d\:0438\:044f G \:043f\:0440\:0438 \:043d\:0430\:043b\:0438\:0447\:0438\:0438 \:043d\:0443\:043b\:0435\:0432\:044b\:0445 \:0438\:043d\:0434\:0435\:043a\:0441\:043e\:0432 \:043d\:0435 \:043d\:0430 \:043f\:043e\:0441\:043b\:0435\:0434\:043d\:0438\:0445 \:043c\:0435\:0441\:0442\:0430\:0445*)


(* ::Text:: *)
(*\:041f\:0440\:0438 \:043d\:0430\:043b\:0438\:0447\:0438\:0438 \:043d\:0443\:043b\:0435\:0432\:044b\:0445 \:0438\:043d\:0434\:0435\:043a\:0441\:043e\:0432 \:043d\:0443\:0436\:043d\:043e \:0432\:044b\:043f\:043e\:043b\:043d\:0438\:0442\:044c \:0440\:0430\:0437\:043b\:043e\:0436\:0435\:043d\:0438\:0435 \:0434\:043e \:0431\:043e\:043b\:0435\:0435 \:0432\:044b\:0441\:043e\:043a\:043e\:0433\:043e \:043f\:043e\:0440\:044f\:0434\:043a\:0430, \:0430 \:0434\:0430\:043b\:044c\:0448\:0435 \:0434\:0435\:0439\:0441\:0442\:0432\:043e\:0432\:0430\:0442\:044c \:043f\:043e \:043e\:0441\:043d\:043e\:0432\:043d\:043e\:043c\:0443 \:0430\:043b\:0433\:043e\:0440\:0438\:0442\:043c\:0443*)


Global`G /: Series[Global`G[idxs:{as___, a_}, z_], {z_, 0, order_}] := With[{l = Length[idxs]}, (
	MapAt[Together, #1, {3, All}]&)[
		Fold[
			Integrate[#1/(z - #2), z] &,
			Integrate[Series[1/(z - a), {z, 0, Max[order - l + Count[idxs, 0], 0]}], z], 
			Reverse[{as}]
		]
	]
]/;a=!=0


(* ::Section:: *)
(*\:0412\:043e\:0437\:043c\:043e\:0436\:043d\:043e\:0441\:0442\:044c \:043c\:0430\:0441\:0448\:0442\:0430\:0431\:0438\:0440\:043e\:0432\:0430\:043d\:0438\:044f \:0438\:043d\:0434\:0435\:043a\:0441\:043e\:0432 \:0438 \:0430\:0440\:0433\:0443\:043c\:0435\:043d\:0442\:0430 \:0432 \:0441\:043b\:0443\:0447\:0430\:0435 \:043d\:0435\:043d\:0443\:043b\:0435\:0432\:043e\:0433\:043e \:043f\:043e\:0441\:043b\:0435\:0434\:043d\:0435\:0433\:043e \:0438\:043d\:0434\:0435\:043a\:0441\:0430*)


(* ::Text:: *)
(*\:0415\:0441\:043b\:0438 \:0438\:043d\:0434\:0435\:043a\:0441\:044b G \:043d\:0430 \:043f\:043e\:0441\:043b\:0435\:0434\:043d\:0438\:0445 \:043c\:0435\:0441\:0442\:0430\:0445 \:043d\:0435\:043d\:0443\:043b\:0435\:0432\:044b\:0435, \:0442\:043e \:043c\:043e\:0436\:043d\:043e \:043e\:0434\:043d\:043e\:0432\:0440\:0435\:043c\:0435\:043d\:043d\:043e \:043c\:0430\:0441\:0441\:0448\:0442\:0430\:0431\:0438\:0440\:043e\:0432\:0430\:0442\:044c \:0430\:0440\:0433\:0443\:043c\:0435\:043d\:0442 \:0438 \:0438\:043d\:0434\:0435\:043a\:0441\:044b \:043d\:0430 \:043e\:0434\:0438\:043d \:0438 \:0442\:043e\:0442 \:0436\:0435 \:0444\:0430\:043a\:0442\:043e\:0440*)


rule`scaleG[factor_] := G[l_List, \[Xi]_] :> G[l * factor,\[Xi] * factor] /; Last[l] =!= 0
ScaleG[factor_] := expr \[Function] ReplaceAll[expr, rule`scaleG[factor]]


End[]


EndPackage[]


Print[ToString@StringForm["[info]: '``' loaded", FileNameTake[$InputFileName, -3]]];
