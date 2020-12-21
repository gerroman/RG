(* ::Package:: *)

(* ::Text:: *)
(*Custom notation and functions to define it*)


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
\[ScriptCapitalM]::usage = "symbol for matrix element";


energy::usage = "
  energy[p] represent time part of 4-vector
";

momentum::usage = "
  momentum[p] represent spatial part of 4-vector 
";

mass::usage = "
  mass[p] represent mass of 4-vector (i.e. energy[p]^2 - abs[momentum[p]]^2 == mass[p]^2) of 4-vector
";
Global`m::usage = "symbol for mass";


abs::usage = "
  abs[p] represent module of spatial vector p
";


sp::usage = "
  sp[a, b] represent scalar product of a, b
";

Global`g::usage = "
  g is symbol for metric tensor
"

setLorentzIndex::usage = "
  setLorentzIndex[mu, ...] format mu as Lorentz index
";
lorentzIndexes::usage = "
  lorentzIndexes return list of current symbols used for Lorentz indexes
";

\[Gamma]::usage = "
  \[Gamma][\[Mu]] represents gamma matrix
";


id::usage = "
  id represents identity matrix
";

zero::usage = "
  zero represent zero matrix
";

theta::usage = "
  theta \[LongDash] polar angle
";
\[Theta]::usage = "
  \[Theta] \[LongDash] symbol for polar angle
";

phi::usage = "
  phi \[LongDash] azimuthal angle
";
\[CurlyPhi]::usage = "
  \[CurlyPhi] \[LongDash] symbol for azimuthal angle 
";

omega::usage = "
  omega \[LongDash] body angle
";
\[CapitalOmega]::usage = "
  \[CapitalOmega] \[LongDash] symbol for spherical angle
";


u::usage = "
  u[p] represent initial particle bispinor
";
v::usage = "
  v[p] represent final anti-particle bispinor
";
bar`u::usage = "
  bar`u represent final particle bispinor
";
bar`v::usage = "
  bar`v represent initial anti-particle bispinor
";


crossSection::usage = "
  crossSection[\"tag\"] or crossSection[\"tag1\", \"tag2\"] represent cross section
";
\[Sigma]::usage = "symbol for cross sections";

Global`d::usage = "
  d symbol to use as differential
";


pcms::usage = "
  pcms symbol for intitial particles momentum in the center of mass frame for the process 2->2
";
prime`pcms::usage = "
  prime`pcms symbol for final particles momentum in the center of mass frame for the process 2->2
";

\[ScriptP]::usage = "
  \[ScriptP] -- symbol for momentum
";
\[ScriptCapitalE]::usage = "
  \[ScriptCapitalE] -- symbol for energy
";
\[ScriptM]::usage = "
  \[ScriptM] -- symbol for energy
";

electron::usage = "
  electron
";
positron::usage = "
  positron
";
Global`e::usage = "
  e symbol for electron
";
Global`\[Alpha]::usage = "
  \[Alpha] symbol for electromagnetic coupling constant
";
rule`alpha::usage = "
  rule`alpha substitute powers of electron charge to electromagnetic coupling constant
";

bar`e::usage = "
  bar`e symbol for positron
";
photon::usage = "
  photon
";
muon::usage = "
  muon
";
antimuon::usage = "
  antimuon
";
Global`\[Mu]::usage = "
  \[Mu] symbol for muon
";
bar`\[Mu]::usage = "
  bar`mu symbol for antimuon
";

plus::usage = "
  plus -- string for sign + 
";
minus::usage = "
  minus -- string for sign -
";
plus\[LetterSpace]minus::usage = "
  plus\[LetterSpace]minus -- list of strign for signs  {+, -}
";

\[Eta]::usage = "
  \[Eta] -- symbol for fermion antiparticle spinor
";

\[Xi]::usage = "
  \[Xi] -- symbol for fermion particle spinor 
";

\[GothicP]::usage = "
  \[GothicP] -- symbol for 4-momentum
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

setIndexed[matrixElement];
matrixElement /: Format[matrixElement, TraditionalForm] = (
  \[ScriptCapitalM]
);


setIndexed[mass];
mass /: Format[mass, TraditionalForm] = Global`m;

energy /: Format[energy[expr_], TraditionalForm] := Superscript[expr, 0];

SetAttributes[sp, Orderless];
sp /: Format[sp[expr_Symbol], TraditionalForm] := Superscript[expr, 2];
sp /: Format[sp[expr_Symbol, expr_], TraditionalForm] := Superscript[expr, 2];
sp /: Format[sp, TraditionalForm] := "";

abs /: Format[abs[expr_], TraditionalForm] := BracketingBar[expr];


momentum /: Format[momentum[expr_], TraditionalForm] := Style[expr, Bold, Italic];


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
  If[Context[mu] == "Global`", (
    With[{primeMu = setPrime[mu]}, setLorentzIndex[primeMu]]
  )];
  lorentzIndexes
);
setLorentzIndex[mu__] := setLorentzIndex[{mu}];

Format[\[Gamma][mu_], TraditionalForm] := Superscript[\[Gamma], mu];

Unprotect[Times];
  Format[a_ * id, TraditionalForm] := a;
Protect[Times];

zero /: Format[zero, TraditionalForm] := Style[0, Bold];

Format[id, TraditionalForm] := Style[1, Bold];


setIndexed[\[Theta], \[CurlyPhi]];
Format[theta,  TraditionalForm] := \[Theta];
Format[phi, TraditionalForm] := \[CurlyPhi];
Format[omega, TraditionalForm] := \[CapitalOmega];


setBar[u, v];


setIndexed[crossSection];
Format[crossSection, TraditionalForm] := \[Sigma];


Global`d/:Format[Global`d[expr_], TraditionalForm]:=HoldForm[Dt[expr]];


setIndexed[\[ScriptP], \[ScriptCapitalE], \[ScriptM]];
pcms /: Format[pcms, TraditionalForm] := \[ScriptP]["cms"];
prime`pcms /: Format[prime`pcms, TraditionalForm] := \[ScriptP]["cms", "\[Prime]"];

electron /: Format[electron, TraditionalForm] = Global`e;
positron /: Format[positron, TraditionalForm] = OverBar[Global`e];

muon /: Format[muon, TraditionalForm] = Global`\[Mu];
antimuon /: Format[antimuon, TraditionalForm] = OverBar[Global`\[Mu]];
photon /: Format[photon, TraditionalForm] = \[Gamma];


rule`alpha = Global`e^(p_) :> (4 \[Pi] Global`\[Alpha])^(p / 2);


Format[u[a_, b_], TraditionalForm] := Subsuperscript[u, a, b];
Format[v[a_, b_], TraditionalForm] := Subsuperscript[v, a, b];
Format[bar`u[a_, b_], TraditionalForm] := Subsuperscript[bar`u, a, b];
Format[bar`v[a_, b_], TraditionalForm] := Subsuperscript[bar`v, a, b];


plus = "+";
minus = "-";
plus\[LetterSpace]minus = {plus, minus};


setIndexed[\[Xi], \[Eta]];
setIndexed[\[GothicP]];


Protect[
  \[ScriptCapitalM], Global`m, Global`g, Global`e, \[Theta], \[CurlyPhi], \[CapitalOmega], \[Sigma], u, v, bar`u, bar`v,
  \[Xi], \[Eta],
  \[GothicP],
  \[ScriptP], \[ScriptCapitalE], \[ScriptM],
  Global`d, pcms, prime`pcms, Global`\[Mu], bar`e, bar`\[Mu],
  electron, positron, muon, antimuon,
  Global`\[Alpha]
];


End[];


EndPackage[];