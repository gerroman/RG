(* ::Package:: *)

BeginPackage["RG`Notation`"]


setIndexed::usage = "setIndexed[x] set symbol x as indexed variable, i.e. x[i], x[i, j] will have sub- and superscripts in the TraditionalForm"


setSuperscript::usage = "setSuperscript[x] set symbol x as superscripted variable, i.e. x[i] will have superscripts in the TraditionalForm"


setSubscript::usage = "setSubscript[x] set symbol x as subscripted variable, i.e. x[i] will have subscripts in the TraditionalForm"


(* setPrime::usage = " *)
(*   setPrime[x] set symbol prime`x in TraditionalForm to have prime (') as superscript *)
(* "; *)


(* setBar::usage = " *)
(*   setBar[x] set symbol bar`x in the TraditionalForm to have overbar *)
(* "; *)


(* setHat::usage = " *)
(*   setHat[x] set symbol hat`x in the TraditionalForm to have overhat *)
(* "; *)


(* setVec::usage = " *)
(*   setVec[x] set symbol vec`x to be bold in the TraditionalForm *)
(* "; *)


(* setTilde::usage = " *)
(*   setTilde[x] set symbol tilde`x in the TraditionalForm to have overtilde *)
(* "; *)


Begin["`Private`"]


SetAttributes[setSubscript, {Listable}];
setSubscript[x_Symbol] := (
  x /: Format[x[i_List], TraditionalForm] := DisplayForm[SubscriptBox[ToBoxes[x,TraditionalForm], RowBox[ToBoxes[#,TraditionalForm]&/@i]]];
  x /: Format[x[i_], TraditionalForm] := DisplayForm[SubscriptBox[ToBoxes[x,TraditionalForm], ToBoxes[i, TraditionalForm]]];
  x /: Format[x[], TraditionalForm] := DisplayForm[ToBoxes[x,TraditionalForm]];
  x
);
setSubscript[x__] := setSubscript[{x}];


SetAttributes[setSuperscript, {Listable}];
setSuperscript[x_Symbol] := (
  x /: Format[x[i_List], TraditionalForm] := DisplayForm[SuperscriptBox[ToBoxes[x,TraditionalForm], RowBox[ToBoxes[#,TraditionalForm]&/@i]]];
  x /: Format[x[i_], TraditionalForm] := DisplayForm[SuperscriptBox[ToBoxes[x,TraditionalForm], ToBoxes[i,TraditionalForm]]];
  x /: Format[x[], TraditionalForm] := ToBoxes[x,TraditionalForm];
  x
);
setSuperscript[x__] := setSuperscript[{x}];


SetAttributes[setIndexed, {Listable}];
setIndexed[x_Symbol] := (
  setSubscript[x];
  x /: Format[x[i_List, j_List], TraditionalForm] := DisplayForm[SubsuperscriptBox[ToBoxes[x,TraditionalForm], RowBox[ToBoxes[#,TraditionalForm]&/@i], RowBox[ToBoxes[#,TraditionalForm]&/@j]]];
  x /: Format[x[i_, j_List], TraditionalForm] := DisplayForm[SubsuperscriptBox[ToBoxes[x,TraditionalForm], ToBoxes[i,TraditionalForm], RowBox[ToBoxes[#,TraditionalForm]&/@j]]];
  x /: Format[x[i_List, j_], TraditionalForm] := DisplayForm[SubsuperscriptBox[ToBoxes[x,TraditionalForm], RowBox[ToBoxes[#,TraditionalForm]&/@i], ToBoxes[j,TraditionalForm]]];
  x /: Format[x[i_, j_], TraditionalForm] := DisplayForm[SubsuperscriptBox[ToBoxes[x,TraditionalForm], ToBoxes[i,TraditionalForm], ToBoxes[j,TraditionalForm]]];
  x /: Format[x[], TraditionalForm] := DisplayForm[ToBoxes[x,TraditionalForm]];
  x
);
setIndexed[x__Symbol] := setIndexed[{x}];


(* SetAttributes[setTilde, {Listable}]; *)
(* setTilde[x_Symbol] := With[{ *)
(*     symbol = ToExpression["tilde`" <> ToString[x]] *)
(*   }, *)
(*   symbol /: Format[symbol, TraditionalForm] = HoldForm[OverTilde[x]]; *)
(* 	symbol *)
(* ]; *)
(* setTilde[x__] := setTilde[{x}]; *)


(* SetAttributes[setVec, {Listable}]; *)
(* setVec[x_Symbol] := With[{ *)
(*     symbol = ToExpression["vec`" <> ToString[x]] *)
(*   }, *)
(*   symbol /: Format[symbol, TraditionalForm] = OverVector[x]; *)
(* 	symbol *)
(* ]; *)
(* setVec[x__] := setVec[{x}]; *)


(* SetAttributes[setPrime, {Listable}]; *)
(* setPrime[x_Symbol] := With[{ *)
(*     symbol = ToExpression["prime`" <> ToString[x]] *)
(*   }, *)
(*   symbol /: Format[symbol, TraditionalForm] = Superscript[x, "\[Prime]"]; *)
(*   symbol *)
(* ]; *)
(* setPrime[x__] := setPrime[{x}]; *)


(* SetAttributes[setBar, {Listable}]; *)
(* setBar[x_Symbol] := With[{ *)
(*     symbol = ToExpression["bar`" <> ToString[x]] *)
(*   }, *)
(*   symbol /: Format[symbol, TraditionalForm] = HoldForm[OverBar[x]]; *)
(*   symbol *)
(* ]; *)
(* setBar[x__] := setBar[{x}]; *)


(* SetAttributes[setHat, {Listable}]; *)
(* setHat[x_Symbol] := With[{ *)
(*     symbol = ToExpression["hat`" <> ToString[x]] *)
(*   }, *)
(*   symbol /: Format[symbol, TraditionalForm] = HoldForm[OverHat[x]]; *)
(*   symbol *)
(* ]; *)
(* setHat[x__] := setHat[{x}]; *)


End[]


EndPackage[]


Print[ToString@StringForm["[info]: '``' loaded", $InputFileName]];
