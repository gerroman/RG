(* ::Package:: *)

(* ::Text:: *)
(*Custom notation and functions to define it*)
BeginPackage["RG`Notation`", {"RG`CommonNotation`"}]

matrixElement::usage = "
  matrixElement[\"tag\"] or matrixElement[\"tag1\", \"tag2\"] represent matrix element
";
\[ScriptCapitalM]::usage = "
  \[ScriptCapitalM] \[LongDash] symbol for matrix element
";


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
  g \[LongDash] symbol for metric tensor
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
  theta[x, y] \[LongDash] polar angle between x and y 3D-vectors
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
  \[CapitalOmega] \[LongDash] symbol for solid angle
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

pcms::usage = "
  pcms symbol for intitial particles momentum in the center of mass frame for the process 2->2
";
prime`pcms::usage = "
  prime`pcms symbol for final particles momentum in the center of mass frame for the process 2->2
";

\[ScriptP]::usage = "
  \[ScriptP] \[LongDash] symbol for momentum
";
\[ScriptCapitalE]::usage = "
  \[ScriptCapitalE] \[LongDash] symbol for energy
";
\[ScriptM]::usage = "
  \[ScriptM] \[LongDash] symbol for mass
";

electron::usage = "
  electron
";
positron::usage = "
  positron
";
proton::usage = "
  proton
";
Global`e::usage = "
  e symbol for electron
";
Global`\[Alpha]::usage = "
  \[Alpha] symbol for electromagnetic coupling constant
";
Global`p::usage = "
 p symbol for proton
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
  plus \[LongDash] string for sign + 
";
minus::usage = "
  minus \[LongDash] string for sign -
";
plus\[LetterSpace]minus::usage = "
  plus\[LetterSpace]minus \[LongDash] list of strign for signs  {+, -}
";

\[Eta]::usage = "
  \[Eta] \[LongDash] symbol for fermion antiparticle spinor
";

\[Xi]::usage = "
  \[Xi] \[LongDash] symbol for fermion particle spinor 
";

\[GothicP]::usage = "
  \[GothicP] \[LongDash] symbol for 4-momentum
";


pion::usage = "
  pion
";


Begin["`Private`"]


Echo["[Info]: Notation.wl allow to use Replace(|All|Repeated)[Equal[...]], "];


Unprotect[Replace, ReplaceAll, ReplaceRepeated];
Replace[eq_Equal] := Replace[Rule @@ eq];
Replace[eqs : {_Equal ..}] := Replace[Rule @@@ eqs];
Replace[expr_, eq_Equal] := Replace[expr, Rule @@ eq];
Replace[expr_, eqs : {_Equal ..}] := Replace[expr, Rule @@@ eqs];

ReplaceAll[eq_Equal] := ReplaceAll[Rule @@ eq];
ReplaceAll[eqs : {_Equal ..}] := ReplaceAll[Rule @@@ eqs];
ReplaceAll[expr_, eq_Equal] := ReplaceAll[expr, Rule @@ eq];
ReplaceAll[expr_, eqs : {_Equal ..}] := ReplaceAll[expr, Rule @@@ eqs];

ReplaceRepeated[expr_, eq_Equal] := ReplaceRepeated[expr, Rule @@ eq];
ReplaceRepeated[expr_, eqs : {_Equal ..}] := ReplaceRepeated[expr, Rule @@@ eqs];

Protect[Replace, ReplaceAll, ReplaceRepeated];


setIndexed[matrixElement];
matrixElement /: Format[matrixElement, TraditionalForm] = (
  \[ScriptCapitalM]
);

setIndexed[mass];
mass /: Format[mass, TraditionalForm] = Global`m;


(* [NOTE: 2021-06-01] Try to use another notation for energy *)
setIndexed[energy];
energy /: Format[energy, TraditionalForm] = \[ScriptCapitalE];


SetAttributes[sp, Orderless];
sp /: Format[sp[expr_Symbol], TraditionalForm] := Superscript[expr, 2];
sp /: Format[sp[expr_Symbol, expr_Symbol], TraditionalForm] := Superscript[expr, 2];
sp /: Format[sp[expr_], TraditionalForm] := HoldForm[expr]^2;
sp /: Format[sp[expr_, expr_], TraditionalForm] := HoldForm[expr]^2;
sp /: Format[sp, TraditionalForm] := "";


abs /: Format[abs[expr_], TraditionalForm] := HoldForm[BracketingBar[expr]];


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
  If[Context[mu] =!= "prime`", (
    With[{primeMu = setPrime[mu]}, setLorentzIndex[primeMu]]
  )];
  lorentzIndexes
);
setLorentzIndex[mu__] := setLorentzIndex[{mu}];


setSuperScript[\[Gamma]];


Unprotect[Times];
  Format[a_ * id, TraditionalForm] := a;
Protect[Times];


zero /: Format[zero, TraditionalForm] = Style[0, Bold];
id /: Format[id, TraditionalForm] = Style[1, Bold];


Format[theta,  TraditionalForm] := \[Theta];
Format[phi, TraditionalForm] := \[CurlyPhi];
Format[omega, TraditionalForm] := \[CapitalOmega];


setIndexed[crossSection];
Format[crossSection, TraditionalForm] := \[Sigma];


setIndexed[\[ScriptP], \[ScriptCapitalE], \[ScriptM], \[GothicP]];
pcms /: Format[pcms, TraditionalForm] := \[ScriptP]["cms"];
prime`pcms /: Format[prime`pcms, TraditionalForm] := Superscript[Subscript[\[ScriptP], "cms"], "\[Prime]"];

electron /: Format[electron, TraditionalForm] = HoldForm[Global`e];
positron /: Format[positron, TraditionalForm] = HoldForm[OverBar[Global`e]];
proton /: Format[proton, TraditionalForm] = HoldForm[Global`p]
muon /: Format[muon, TraditionalForm] = HoldForm[Global`\[Mu]];
antimuon /: Format[antimuon, TraditionalForm] = HoldForm[OverBar[Global`\[Mu]]];
photon /: Format[photon, TraditionalForm] = HoldForm[\[Gamma]];
pion /: Format[pion, TraditionalForm] = HoldForm[\[Pi]];

setIndexed[electron, positron, proton, muon, antimuon, pion];

rule`alpha = Global`e^(p_) :> (4 \[Pi] Global`\[Alpha])^(p / 2);

setBar[u, v];
setIndexed[u, v, bar`u, bar`v];

plus = "+";
minus = "-";
plus\[LetterSpace]minus = {plus, minus};

setIndexed[\[Xi], \[Eta]];


End[];


Echo[$Context];


EndPackage[];
