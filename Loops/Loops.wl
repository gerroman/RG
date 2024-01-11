(* ::Package:: *)

BeginPackage["RG`Loops`"]


describeDiagram::usage = "
  describeDiagram[l, k] make a description for the l-loops and k-legged diagram
";

den::usage="
  den[d, n] = den[{d, n}] = den[{{d, n}}] \[LongDash] denominator 1/d^n
  den[{{d1, n1}..}] \[LongDash] product of denominators
"

getSector::usage="
  getSector[p] evaluates sector for the point p
";


Begin["`Private`"]


describeDiagram[l_, k_] := Module[
	{e, n, m, r},
	e = Factor[k - 1];
	n = l * (l + 1) / 2 + l * e;
	m = e + 3 l - 2;
	r = Factor[n - m];
	{
		{l, "#(loops), #(loop momenta)"},
		{k, "#(legs), #(external momenta)"},
		{e, "#(independent external momenta)"},
		{n, "#(scalar products depending on loop momenta)"},
		{m, "#(maximum number of denominators)"},
		{r, "#(irreducible numerators)"},
		{2^n, "#(possible sectors)"}
	}
];


denBox[{d_, n_}] := With[{
    dbox = RowBox[{"[", ToBoxes[d, TraditionalForm], "]"}]
  }, 
  If[n =!= 1, SuperscriptBox[dbox, n], dbox]
];
den /: Format[den[{dn:{_,_}}], TraditionalForm] := DisplayForm[FractionBox[1, denBox[dn]]];
den /: Format[den[dl:{{_,_}..}], TraditionalForm] := DisplayForm[FractionBox[1, RowBox[denBox /@ dl]]];

den[a_, b__] := den[{a, b}];
den[l_?VectorQ] := den[{l}];
den[{pre___, {_, 0}, post___}] := den[{pre, post}]
den[{}] = 1;
den /: den[l_List] * den[m_List] := den[
  Sort[Join[l, m]] //. {{pre___, {a_, x_}, {a_, y_}, post___} :> {pre, {a, x + y}, post}}
];


getSector[l_List] := Map[Boole[# - 1/2 > 0]&, l];


End[]


EndPackage[]
