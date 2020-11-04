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


UnderBar::usage = "
  UnderBar[expr] is equivalent for HoldForm[expr]
";


sp::usage = "
  sp[a, b] represent scalar product of a, b
";

setLorentzIndex::usage = "
  setLorentzIndex[mu, ...] format mu as Lorentz index
";


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

setIndexed[matrixElement];
matrixElement /: Format[matrixElement, TraditionalForm] = (
  Global`\[ScriptCapitalM]
);


UnderBar = HoldForm;


SetAttributes[sp, Orderless];
sp /: Format[sp[expr_Symbol], TraditionalForm] := Superscript[expr, 2];
sp /: Format[sp[expr_Symbol, expr_Symbol], TraditionalForm] := Superscript[expr, 2];
sp[expr_] := sp[expr, expr];
sp[a___, b_ * mult_?NumberQ, c___] := mult * sp[a, b, c];


lorentzIndexes = {};
SetAttributes[setLorentzIndex, Listable];
setLorentzIndex[mu_Symbol] := (
  If[FreeQ[lorentzIndexes, mu], (
    Function[expr, 
      Format[sp[expr, mu], TraditionalForm] := Superscript[Global`g, ToString@Row[{expr, mu}]]
    ] /@ lorentzIndexes;
    Format[sp[a_Symbol, mu], TraditionalForm] := Superscript[a, mu];
    Format[sp[a_, mu], TraditionalForm] := Superscript[AngleBracket[a], mu];
    Format[sp[mu, mu], TraditionalForm] := Superscript[Global`g, ToString@Row[{mu, mu}]];
    AppendTo[lorentzIndexes, mu];
  )];
);
setLorentzIndex[mu__] := setLorentzIndex[{mu}];


End[]


EndPackage[]