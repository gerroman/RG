(* ::Package:: *)
(* Functions to define custom notations *)

BeginPackage["RG`Notation`"]


setIndexed::usage = "
  setIndexed[x] set symbol x as indexed variable, \
i.e. x[i], x[i, j] will have sub- and superscripts in Traditional form
";


setPrime::usage = "
  setPrime[x] set symbol prime`x in traditional form to have prime (') as superscript
";


setBar::usage = "
  setBar[x] set symbol bar`x in traditional form to have overbar
";


matrixElement::usage = "
  matrixElement[\"tag\"] or matrixElement[\"tag1\", \"tag2\"] represent matrix element
";
Global`\[ScriptCapitalM]::usage = "symbol for matrix element";


Begin["`Private`"]


SetAttributes[setIndexed, {HoldAll, Listable}];
setIndexed[x_Symbol] := (
  x /: Format[x[i_], TraditionalForm] := Subscript[x, i];
  x /: Format[x["", j_], TraditionalForm] := Superscript[x, j];
  x /: Format[x[i_, j_], TraditionalForm] := Superscript[Subscript[x, i], j];
);
setIndexed[x__] := setIndexed[{x}];


SetAttributes[setPrime, {HoldAll, Listable}];
setPrime[x_Symbol] := With[{
    symbol = ToExpression["prime`" <> ToString[x]]
  },
  symbol /: Format[symbol, TraditionalForm] := Superscript[x, Global`\[Prime]];
];
setPrime[x__] := setPrime[{x}];


SetAttributes[setBar, {HoldAll, Listable}];
setBar[x_Symbol] := With[{
    symbol = ToExpression["bar`" <> ToString[x]]
  },
  symbol /: Format[symbol, TraditionalForm] := OverBar[x];
];
setBar[x__] := setBar[{x}];


SetAttributes[protect, HoldAll];
protect::warn = "value of `` will be cleared";
protect[symb_] := (
  If[ValueQ[symb], (
    Message[protect::warn, HoldForm[symb]];
    Unprotect[symb];
    Clear[symb];
  )];
  Protect[symb];
);

protect[Global`\[ScriptCapitalM]];
setIndexed[matrixElement];
matrixElement /: Format[matrixElement, TraditionalForm] = (
  Global`\[ScriptCapitalM]
);


End[]


EndPackage[]