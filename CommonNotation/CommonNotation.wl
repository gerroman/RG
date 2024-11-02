(* ::Package:: *)

BeginPackage["RG`CommonNotation`"]


setIndexed::usage = "
  setIndexed[x] set symbol x as indexed variable, i.e. x[i], x[i, j] will have sub- and superscripts in the TraditionalForm
";

setSuperscript::usage = "
  setSuperscript[x] set symbol x as superscripted variable, i.e. x[i] will have superscripts in the TraditionalForm
";

setSubscript::usage = "
  setSubscript[x] set symbol x as subscripted variable, i.e. x[i] will have subscripts in the TraditionalForm
"

setPrime::usage = "
  setPrime[x] set symbol prime`x in TraditionalForm to have prime (') as superscript
";

setBar::usage = "
  setBar[x] set symbol bar`x in the TraditionalForm to have overbar
";

setHat::usage = "
  setHat[x] set symbol hat`x in the TraditionalForm to have overhat
";
setVec::usage = "
  setVec[x] set symbol vec`x to be bold in the TraditionalForm
";

zero::usage = "
  zero for zero
";

(* minus::usage = " *)
(*   minus for sign minus *)
(* "; *)

(* plus::usage = " *)
(*   plus for sign plus *)
(* "; *)

plusminus::usage = "
  plusminus for both signs
";
minusplus::usage = "
  minusplus for both signs
";

min::usage = "
  min for minimum tag
";

max::usage = "
  max for maximum tag
";


setTilde::usage = "
  setTilde[x] set symbol tilde`x in the TraditionalForm to have overtilde
";


integrate::usage = "
  integrate[expr, region] represent integrals
";


sum::usage = "
  sum[expr, region] represent sums
";


limit::usage = "
  limit[expr, var] represent limits
";


d::usage = "
  d[expr, var] represent differential
";

dt::usage = "
  dt[expr, var] represent full differential
";

pd::usage = "
  pd[expr, var] represent partial differential
";

at::usage = "
  at[expr, {x, x0}] or at[expr, {x, x1, x2}] for limits substitute
";

perp::usage = "
  perp for perpendicular symbol
";


braket::usage = "
  braket[a, b, c] represent bra, ket matrix elemetn
";
bra::usage = "
  bra[a] represent bra vector
";
ket::usage = "
  ket[a] represent bra vector
";


vector::usage = "
  vector[x] represent vector x
";


dots::usage = "
  dots represent \"...\"
";


reIndex::usage = "
  reIndex[expr, pattern, symbol] replace unique symbols with names started with `pattern` to indexed `symbol` in the `expr`
";


Begin["`Private`"]


SetAttributes[setSubscript, {Listable}];
setSubscript[x_Symbol] := (
  x /: Format[x[i_List], TraditionalForm] := DisplayForm[SubscriptBox[x, RowBox[i]]];
  x /: Format[x[i_], TraditionalForm] := Subscript[x, i];
  x /: Format[x[], TraditionalForm] := x;
  x
);
setSubscript[x__] := setSubscript[{x}];


SetAttributes[setSuperscript, {Listable}];
setSuperscript[x_Symbol] := (
  x /: Format[x[i_List], TraditionalForm] := DisplayForm[SuperscriptBox[x, RowBox[i]]];
  x /: Format[x[i_], TraditionalForm] := Superscript[x, i];
  x /: Format[x[], TraditionalForm] := x;
  x
);
setSuperscript[x__] := setSuperscript[{x}];


SetAttributes[setIndexed, {Listable}];
setIndexed[x_Symbol] := (
  setSubscript[x];
  x /: Format[x[i_List, j_List], TraditionalForm] := DisplayForm[SubsuperscriptBox[x, RowBox[i], RowBox[j]]];
  x /: Format[x[i_, j_List], TraditionalForm] := DisplayForm[SubsuperscriptBox[x, i, RowBox[j]]];
  x /: Format[x[i_List, j_], TraditionalForm] := DisplayForm[SubsuperscriptBox[x, RowBox[i], j]];
  x /: Format[x[i_, j_], TraditionalForm] := DisplayForm[SubsuperscriptBox[x, i, j]];
  x /: Format[x[], TraditionalForm] := x;
  x
);
setIndexed[x__Symbol] := setIndexed[{x}];


SetAttributes[setTilde, {Listable}];
setTilde[x_Symbol] := With[{
    symbol = ToExpression["tilde`" <> ToString[x]]
  },
  symbol /: Format[symbol, TraditionalForm] = HoldForm[OverTilde[x]];
	symbol
];
setTilde[x__] := setTilde[{x}];

SetAttributes[setVec, {Listable}];
setVec[x_Symbol] := With[{
    symbol = ToExpression["vec`" <> ToString[x]]
  },
  symbol /: Format[symbol, TraditionalForm] = OverVector[x];
	symbol
];
setVec[x__] := setVec[{x}];

SetAttributes[setPrime, {Listable}];
setPrime[x_Symbol] := With[{
    symbol = ToExpression["prime`" <> ToString[x]]
  },
  symbol /: Format[symbol, TraditionalForm] = Superscript[x, "\[Prime]"];
  symbol
];
setPrime[x__] := setPrime[{x}];


SetAttributes[setBar, {Listable}];
setBar[x_Symbol] := With[{
    symbol = ToExpression["bar`" <> ToString[x]]
  },
  symbol /: Format[symbol, TraditionalForm] = HoldForm[OverBar[x]];
  symbol
];
setBar[x__] := setBar[{x}];


SetAttributes[setHat, {Listable}];
setHat[x_Symbol] := With[{
    symbol = ToExpression["hat`" <> ToString[x]]
  },
  symbol /: Format[symbol, TraditionalForm] = HoldForm[OverHat[x]];
  symbol
];
setHat[x__] := setHat[{x}];


(* minus /: Format[minus, TraditionalForm] = "-"; *)
(* plus /: Format[plus, TraditionalForm] = "+"; *)
plusminus /: Format[plusminus, TraditionalForm] = "\[PlusMinus]";
minusplus /: Format[minusplus, TraditionalForm] = "\[MinusPlus]";
zero /: Format[zero, TraditionalForm] = "0";
min /: Format[min, TraditionalForm] = "min";
max /: Format[max, TraditionalForm] = "max";


integrate /: Format[integrate[expr_, {{l_, dim_}}], TraditionalForm] := DisplayForm[
  RowBox[
	{
	  "\[Integral]",
    RowBox[{SuperscriptBox["\[DifferentialD]", TraditionalForm[dim]], l}],
	  expr
	}]
];
integrate /: Format[integrate[expr_, ls:{{_, _}..}], TraditionalForm] := With[
  {lbs = Apply[RowBox[{SuperscriptBox["\[DifferentialD]", #2], #1}]&] /@ ls},
  DisplayForm[RowBox[{"\[Integral]", Sequence@@lbs, expr}]]
];
integrate /: Format[integrate[expr_, {{l1_, dim1_}, dots, {l2_, dim2_}}], TraditionalForm] := DisplayForm[
  RowBox[{
	 "\[Integral]",
   RowBox[{SuperscriptBox["\[DifferentialD]", dim1], l1}],
	 "\[Ellipsis]",
   RowBox[{SuperscriptBox["\[DifferentialD]", dim2], l2}],
	 expr
	}]
];
integrate /: Format[integrate[expr_, region__], TraditionalForm] := (
  HoldForm[Integrate[expr, region]]
);
integrate /: Format[integrate[expr_], TraditionalForm] := (
  StringForm["\[Integral]``", expr] // ToString
);


sum /: Format[sum[expr_, region__], TraditionalForm] := (
  HoldForm[Sum[expr, region]]
);
sum /: Format[sum[expr_], TraditionalForm] := (
  StringForm["\[Sum]``", expr] // ToString
);


limit /: Format[limit[expr_, args_], TraditionalForm] := HoldForm[Limit[expr, args]];


SetAttributes[d, HoldAll];
d /: Format[d[expr_, args_], TraditionalForm] := HoldForm[D[expr, args]];
d /: Format[d[expr_], TraditionalForm] := HoldForm[Dt[expr]];

SetAttributes[dt, HoldAll];
dt /: Format[dt[expr_, args_], TraditionalForm] := HoldForm[Dt[expr, args]];


SetAttributes[pd, HoldAll];
pd /: Format[pd[expr_], TraditionalForm] := DisplayForm[RowBox[{"\[PartialD]", expr}]];
pd /: Format[pd[expr_, arg_Symbol], TraditionalForm] := DisplayForm[
  RowBox[{
	  SubscriptBox["\[PartialD]", ToBoxes[arg, TraditionalForm]],
		" ",
	  ToBoxes[expr, TraditionalForm]
	}]
];
pd /: Format[pd[expr_, arg_], TraditionalForm] := HoldForm[D[expr, HoldForm[arg]]];


at /: Format[at[expr_, {x_, y_}], TraditionalForm] :=
  HoldForm[Subscript[""[expr], x -> y]]
at /: Format[at[expr_, {x_, y_, z_}], TraditionalForm] :=
  HoldForm[Subsuperscript[""[expr], x -> y, x -> z]]


perp = "\[Perpendicular]";

Format[braket[a_, b__, c_], TraditionalForm] := HoldForm[Times["\[LeftAngleBracket]",a,"|", b, "|", c,"\[RightAngleBracket]"]]

bra=Bra;
ket=Ket;


vector /: Format[vector[expr_], TraditionalForm] := Style[expr, Bold, Italic];


dots /: Format[dots, TraditionalForm] := "...";


reIndex[expr_, pattern_String, symbol_Symbol] := With[{
    syms = (expr //
      Cases[#, (_Symbol)?(StringMatchQ[SymbolName[#], pattern~~__]&), Infinity]& //
      Union
    )
  },
  expr // ReplaceAll[Thread[syms -> Array[symbol,Length[syms]]]]
]


End[]


(* Print[$Context]; *)


EndPackage[]


Print[ToString@StringForm["[info]: '``' loaded", FileNameTake[$InputFileName, -3]]];
