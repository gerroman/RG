(* ::Package:: *)


BeginPackage["RG`CommonNotation`"]


setIndexed::usage = "
  setIndexed[x] set symbol x as indexed variable, \
i.e. x[i], x[i, j] will have sub- and superscripts in the TraditionalForm
";


setPrime::usage = "
  setPrime[x] set symbol prime`x in TraditionalForm to have prime (') as superscript
";


setBar::usage = "
  setBar[x] set symbol bar`x in the TraditionalForm to have overbar
";


setTilde::usage = "
  setTilde[x] set symbol tilde`x in the TraditionalForm to have overtilde
";


setSuperscript::usage = "
  setSuperscript[x] set symbol x as superscripted variable, \
i.e. x[i] will have superscripts in the TraditionalForm
"


integrate::usage = "
  integrate[expr, region] represent integrals
";


sum::usage = "
  sum[expr, region] represent sums
";


Begin["`Private`"]


SetAttributes[setIndexed, {HoldAll, Listable}];
setIndexed[x_Symbol] := (
  x /: Format[x[i_], TraditionalForm] := Subscript[x, i];
  x /: Format[x["", j_], TraditionalForm] := Superscript[x, j];
  x /: Format[x[i_, j_], TraditionalForm] := Subsuperscript[x, i, j];
  x /: Format[x[i_, j_], TeXForm] := Superscript[Subscript[x, i], j];
);
setIndexed[x__] := setIndexed[{x}];


SetAttributes[setSuperscript, {HoldAll, Listable}];
setSuperscript[x_Symbol] := (
  x /: Format[x[i_], TraditionalForm] := Superscript[x, i];
);
setSuperscript[x__] := setSuperscript[{x}];


SetAttributes[setTilde, {HoldAll, Listable}];
setTilde[x_Symbol] := With[{
    symbol = ToExpression["tilde`" <> ToString[x]]
  },
  symbol /: Format[symbol, TraditionalForm] := HoldForm[OverTilde[x]];
  symbol
];
setTilde[x__] := setTilde[{x}];


SetAttributes[setPrime, {HoldAll, Listable}];
setPrime[x_Symbol] := With[{
    symbol = ToExpression["prime`" <> ToString[x]]
  },
  symbol /: Format[symbol, TraditionalForm] := Superscript[x, Global`\[Prime]];
  symbol
];
setPrime[x__] := setPrime[{x}];


SetAttributes[setBar, {HoldAll, Listable}];
setBar[x_Symbol] := With[{
    symbol = ToExpression["bar`" <> ToString[x]]
  },
  symbol /: Format[symbol, TraditionalForm] := OverBar[x];
  symbol
];
setBar[x__] := setBar[{x}];


integrate/:Format[integrate[expr_, region__], TraditionalForm] := HoldForm[Integrate[expr, region]];
integrate/:Format[integrate[expr_], TraditionalForm] := StringForm["\[Integral]``", expr];


sum/:Format[sum[expr_, region__], TraditionalForm] := HoldForm[Sum[expr, region]];
sum/:Format[sum[expr_], TraditionalForm] := StringForm["\[Sum]``", expr];


End[]


EndPackage[]
