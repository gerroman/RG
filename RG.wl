(* ::Text:: *)
(*RG.wl: base file to load all subpackages and definitions *)


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


Needs /@ {
  "RG`BaseUtils`"
  , "RG`Presentation`"
  , "RG`Calculation`"
  , "RG`Notation`"
  , "RG`Kinematics`"
  , "RG`Traces`"
  , "RG`FeynmanDiagrams`"
  , "RG`Particles`"
  (* , "RG`HelicityStates`"*)
};
