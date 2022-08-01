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

minus::usage = "
  minus for sign minus
";

plus::usage = "
  plus for sign plus
";

plusminus::usage = "
  plusminus for both signs
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


Begin["`Private`"]


SetAttributes[setSubscript, {Listable}];
setSubscript[x_Symbol] := (
  x /: Format[x[i_List], TraditionalForm] := Subscript[x, Row[i]];
  x /: Format[x[i_List], TeXForm] := Subscript[x, ToString@Row[i, ","]];
  x /: Format[x[i_], TraditionalForm] := Subscript[x, i];
	x /: Format[x[i__], TraditionalForm] := Subscript[x, Row[{i}]];
  x /: Format[x[], TraditionalForm] := x;
  x
);
setSubscript[x__] := setSubscript[{x}];


SetAttributes[setSuperscript, {Listable}];
setSuperscript[x_Symbol] := (
  x /: Format[x[i_List], TraditionalForm] := Superscript[x, Row[i]];
  x /: Format[x[i_List], TeXForm] := Superscript[x, ToString@Row[i, ","]];
  x /: Format[x[i_], TraditionalForm] := Superscript[x, i];
  x /: Format[x[], TraditionalForm] := x;
  x
);
setSuperscript[x__] := setSuperscript[{x}];


SetAttributes[setIndexed, {Listable}];
setIndexed[x_Symbol] := (
  setSubscript[x];
  x /: Format[x[i_List, j_List], TraditionalForm] := Subsuperscript[x,Row[i], Row[j]];
  x /: Format[x[i_List, j_List], TeXForm] := Subsuperscript[x, ToString@Row[i, ","], ToString@Row[j, ","]];
  x /: Format[x[i_, j_List], TraditionalForm] := Subsuperscript[x, i, Row[j]];
  x /: Format[x[i_, j_List], TeXForm] := Subsuperscript[x, i, ToString@Row[j, ","]];
  x /: Format[x[i_List, j_], TraditionalForm] := Subsuperscript[x, Row[i], j];
  x /: Format[x[i_List, j_], TeXForm] := Subsuperscript[x, ToString@Row[i, ","], j];
  x /: Format[x[i_, j_], TraditionalForm] := Subsuperscript[x, i, j];
  x /: Format[x[i_, j_], TeXForm] := Superscript[Subscript[x, i], j];
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


minus /: Format[minus] = "-";
plus /: Format[plus] = "+";
plusminus /: Format[plusminus]= "\[PlusMinus]";


min /: Format[min] = "min";
max /: Format[max] = "max";


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


ClearAll[pd];
SetAttributes[pd, HoldAll];
pd /: Format[pd[expr_], TraditionalForm] := HoldForm[Row[{"\[PartialD]", expr}]];
pd /: Format[pd[expr_, arg_Symbol], TraditionalForm] := HoldForm[Row[{Subscript["\[PartialD]", arg], expr}]];
pd /: Format[pd[expr_, arg_], TraditionalForm] := HoldForm[D[expr, HoldForm[arg]]];


ClearAll[at, force];
at /: Format[at[expr_, {x_, y_}], TraditionalForm] :=
  HoldForm[Subscript[""[expr], x -> y]]
at /: Format[at[expr_, {x_, y_, z_}], TraditionalForm] :=
  HoldForm[Subsuperscript[""[expr], x -> y, x -> z]]


End[]


Print[$Context];


EndPackage[]
