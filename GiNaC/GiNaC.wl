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


rule`Li2::usage = "rule`Li2 \[LongDash] rule for useful relations between dilogarithms"
rule`scaleG::usage = "rule`scaleG[factor] \[LongDash] allow to scale Goncharov polylogarithms` argument"

checkComplexRelation::usage = "checkComplexRelation[func1, func2] \[LongDash] check relations between the two functions {func1, func2}"

rndx::usage="rndx[i] reindex tau$.. symbols"

Global`MZV::usage="MZV[m1,...,mn] represent multiple zeta value"
EvalMZV::usage="EvalMZV[{m1, ..., mn}, {s1, ..., sn}] evaluate multiple zeta value"

Global`Li::usage="Li[{m1, ..., mn}, {x1, ..., xn}] represent multiple polylogarithm"
EvalLi::usage="EvalLi[{m1, ..., mn}, {x1, ..., xn}] evaluate multiple polylogarithm"


Begin["`Private`"];


EvalG::nofile="can not find G.exe to evaluate Goncharov polylogarithms numerically"
With[{fname=FileNameJoin[{"RG", "GiNaC", "bin", "G.exe"}]},
If[FindFile[fname] =!= $Failed, (
    Install[fname];
    Global`G /: N[Global`G[zs_List, y_]] := Chop[Complex @@ EvalG[N@Re[zs], N@Im[zs], N[y]]];
    Global`H /: N[Global`H[ms_List, y_]] := Chop[Complex @@ EvalH[ms, N[y]]];
    Global`MZV /: N[Global`MZV[ns:_Integer..]] := Chop[EvalMZV[Abs[{ns}], Sign[{ns}]]];
    Global`Li /: N[Global`Li[ms:{_Integer..}, xs_List]] := Chop[EvalLi[ms, N[xs]]];
  )
  ,
  Message[EvalG::nofile]
]]


(* ::Section:: *)
(*Представление*)


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
(*Разложение G при отсутствии нулевых индексов*)


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
(*Преобразование функции G при наличии нулевых индексов на последних местах*)


(* ::Text:: *)
(*Раскрытие произведения пары G-функций (shuffle product двух наборов)*)


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
(*Избавление от нулевых индексов на последних местах*)


SetAttributes[ReduceGZeros,Listable]
ReduceGZeros[Global`G[b:{0..}, x_]] := With[{n=Length[b]}, Power[Log[x], n]/n!];
ReduceGZeros[Global`G[{a__, b:0..}, x_]] := (
	(* log[{a, b}]; *)
	ReduceGZeros[Global`G[{b}, x]] * Global`G[{a}, x] 
	- ReduceGZeros[ExpandGProduct[Global`G[{b}, x] Global`G[{a}, x]] - Global`G[{a, b}, x]]
);
ReduceGZeros[a_ + b_] := ReduceGZeros[a] + ReduceGZeros[b];
ReduceGZeros[a_ * b_] := ReduceGZeros[a] * ReduceGZeros[b];
ReduceGZeros[Power[a_, b_]] := ReduceGZeros[a]^b;
ReduceGZeros[a_] := a


(* ::Section:: *)
(*Разложения G при наличии нулевых индексов не на последних местах*)


(* ::Text:: *)
(*При наличии нулевых индексов нужно выполнить разложение до более высокого порядка, а дальше действовать по основному алгоритму*)


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
(*Возможность масштабирования индексов и аргумента в случае ненулевого последнего индекса*)


(* ::Text:: *)
(*Если индексы G на последних местах ненулевые, то можно одновременно массштабировать аргумент и индексы на один и тот же фактор*)


rule`scaleG[] = {
  Global`G[l_List, x_] :> Global`G[l / x, 1] /; Last[l] =!= 0
}
rule`scaleG[factor_] := {
  Global`G[l_List, x_] :> Global`G[l * factor, x * factor] /; Last[l] =!= 0
}
(* ScaleG[factor_] := expr \[Function] ReplaceAll[expr, rule`scaleG[factor]] *)


(* ::Section:: *)
(* Часто используемые функциональные cвойства дилогарифмов *)


rule`Li2 = {
  (*1: reflection-1 *) PolyLog[2, z_] :> -PolyLog[2, 1 - z] + Pi^2/6 - Log[1-z] * Log[z],
  (*2: reflection-2 *) PolyLog[2, z_] :> -PolyLog[2, 1 / z] - Pi^2/6 - 1/2 * Log[-z]^2,
  (*4: reflection-3 *) PolyLog[2, z_] :> -PolyLog[2, -z / (1 - z)] - 1/2 * Log[1-z]^2,
  (*3: duplication  *) PolyLog[2, z_] :> -PolyLog[2, -z] + 1/2 * PolyLog[2, z^2],
  (*5: reflection - dupliction *) PolyLog[2, z_] :> (
    PolyLog[2, 1 + z] - 1/2 *PolyLog[2, 1 - z^2] - Pi^2/12
    + Log[1 + z] * Log[-z] - 1/2 Log[1 - z^2] * Log[z^2]
  ),
  (*6: real-argument-1 *) PolyLog[2, x_/;(-1<=x<=1)] :> -PolyLog[2, (1-x)/(1+x)] + PolyLog[2,-(1-x)/(1+x)] + Pi^2/4 + PolyLog[2,-x] + Log[x] * Log[(1+x)/(1-x)],
  (*7: real-argument-2 *) PolyLog[2, x_/;(x > 1)] :> -PolyLog[2, 1/x] + Pi^2/3 - 1/2*Log[x]^2 - I * Pi * Log[x]
}


checkComplexRelation[func1_, func2_, max_:3/2, n1_:9, n2_:16] := With[{
		p1 = Range[0, max, max/n1]
	},
	With[{
			func = Function[{z}, With[{expr1 = func1[z], expr2 = func2[z]},
				Style[{Re[z], Im[z]}, Which[
					HoldForm[expr1] === HoldForm[expr2], Yellow,
					Abs[expr1 - expr2] < 10^(-12), Darker@Green,
					True, Darker@Red
				]]
			]],
			p2 = Union@Flatten@Array[p1 * Exp[I Pi # / n2]&, 2*n2, 0]
		},
		ListPlot[func /@ p2,
			AspectRatio->1,
			PlotRange -> {{-max,max},{-max,max}},
			GridLines->Automatic,
			PlotStyle->{PointSize[Large]},
			Epilog->{Opacity[0.5], Gray, Thick, Circle[]}
		] // Labeled[#, TraditionalForm[func1[Global`z] == func2[Global`z]]] &
	]
]


RG`Tools`reindex[expr_, pattern_, func_] := With[{syms = DeleteDuplicates[Cases[expr, pattern, All]]},
  With[{rules = Thread[syms -> Array[func, Length[syms]]]},
    ReplaceAll[expr, rules]
  ]
];
rndx[i_:0] := RG`Tools`reindex[#,
  s_Symbol/;SymbolName[s]~StringStartsQ~"tau",
  ToExpression[ToString@StringForm["\[Xi]``", #+i]]&
]&


End[]


EndPackage[]


(* Print[ToString@StringForm["[info]: '``' loaded", FileNameTake[$InputFileName, -3]]]; *)
