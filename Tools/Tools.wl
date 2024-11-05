(* ::Package:: *)

(* ::Section:: *)
(*Rules*)


rule`hold::usage = "rule`hold[x] \[LongDash] rules to replace {x -> Hold[x]}"

rule`release::usage = "rule`release[x]  \[LongDash] rules to replace {(Hold|HoldForm)[x]->x}"

rule`powerExpand::usage = "rule`powerExpand[x] \[LongDash] rule to pull x out of Power[] and Abs[]"

rule`factor::usage = "rule`factor[x] \[LongDash] rule to factor x out of Plus[]"

rule`pull::usage = "rule`pull[x] \[LongDash] rule to pull x out of Plus[]"

rule`group::usage = "rule`group[x, func] \[LongDash] rule to replace {func[x] -> x}"


Begin["rule`Private`"]


rule`hold[xs_List] := Join[{expr_Hold :> expr}, Thread[xs -> Hold/@xs]]
rule`hold[xs__] := rule`hold[{xs}]


rule`release[xs_List] := ((Hold|HoldForm)[#]->#)& /@ xs
rule`release[xs__] := rule`release[{xs}]


rule`powerExpand[xs_List] := Flatten[{
  (expr_. * #^p_.)^q_ :> expr^q * #^(p q), 
  Abs[expr_. * #^p_.] :> Abs[expr] * #^p
}& /@ xs]
rule`powerExpand[xs__] := rule`powerExpand[{xs}]


rule`factor[x_] := Plus[expr:(x * _.), other:(x * _.)..] :> x Plus@@({expr, other}/x)
rule`pull[x_] := Plus[expr:(x * _.), other__] :> x Plus@@({expr, other}/x)


rule`group[x_, func_] := func[x] -> x


End[]


(* ::Section:: *)
(*Tools*)


BeginPackage["RG`Tools`"]


hold::usage="hold[x] \[LongDash] replace {x -> Hold[x]}."

release::usage="release[x] \[LongDash] replace {(Hold|HoldForm)[x]->x}."

powerExpand::usage="powerExpand[x] \[LongDash] pull out x of powers assuming that x is positive."

factorIt::usage="factorIt[x] \[LongDash] factor out x from sums."

changeSign::usage="changeSign[x] \[LongDash] replace x^p -> (-x)^p (-1)^p"

groupIt::usage="groupIt[expr, func] \[LongDash] replace func[expr]->expr"

modify::usage="modify[pattern, func] \[LongDash] replace (expr:pattern) :> func[expr]"

pullIt::usage="pullIt[x] \[LongDash] pull x out of sums."

eq::usage = "eq[expr, func] \[LongDash] form an equation HoldForm[expr] == func[expr]"

composition::usage = "composition[fs][expr] \[LongDash] sequentially apply all functions in the list `fs' to `expr' printing intermediate results (by default) and returning final expression\n[note]: it is equivalent to RightComposition[fs][expr]"

force::usage = "force[at | limit | sum | integrate | d] forces evaluation";

cases::usage = "cases[pattern] \[LongDash] just a shortcut for Union[Cases[#, pattern, Infinity]]&"


Begin["`Private`"];


hold[xs_List] := With[{rule=rule`hold[xs]},	ReplaceRepeated[#, rule]&];
hold[xs__]:=hold[{xs}]


release[xs_List] := With[{rule=rule`release[xs]},	ReplaceRepeated[#, rule]&]
release[xs__]:=release[{xs}]


powerExpand[xs_List] := With[{rule=rule`powerExpand[xs]}, ReplaceRepeated[#, rule]&];
powerExpand[xs__] := powerExpand[{xs}]


factorIt[x_] := With[{rule=rule`factor[x]}, ReplaceRepeated[#, rule]&]
factorIt[xs_List] := RightComposition@@(factorIt/@xs)
factorIt[x_, xs__] := factorIt[{x, xs}]


pullIt[x_] := With[{rule=rule`pull[x]}, ReplaceRepeated[#, rule]&]
pullIt[xs_List] := RightComposition@@(pullIt/@xs)
pullIt[x_, xs__] := pullIt[{x, xs}]


changeSign[xs_List] := With[{hs=Hold/@xs, hms=Hold/@(-xs)}, Function[expr, 
  expr //
	  hold[xs] //
		ReplaceAll[Thread[hs -> (-1)*hms]] //
		powerExpand[hms] //
		release[-xs]
]]
changeSign[xs__] := changeSign[{xs}]


groupIt[x_, func_:Expand] := With[{rule = rule`group[x, func]},
  ReplaceRepeated[#, rule]&
]
groupIt[xs_List, func_:Expand] := With[{rule = (rule`group[#, func])& /@ xs},
  ReplaceRepeated[#, rule]&
]


modify[pattern_, func_:Expand] := With[{rule = (ex:pattern) :> func[ex]},
  ReplaceRepeated[#, rule]&
]


SetAttributes[eq, HoldFirst]
Options[eq] = {HoldForm->True};
eq[expr_, fs_List:{Identity}, opts:OptionsPattern[]] := With[
	{
		func = RightComposition@@fs,
    lhs = If[OptionValue[HoldForm], HoldForm[expr], expr]
  },
	lhs == func[expr]
]
eq[expr_, lfs_List, rfs_List, opts:OptionsPattern[]] := With[
	{
    lfunc = RightComposition@@lfs,
		rfunc = RightComposition@@rfs
  },
	lfunc[expr] == rfunc[expr]
]
eq[expr_, fs__, opts:OptionsPattern[]] := eq[expr, {fs}, opts]


Options[composition] = {"verbose"->False}
composition[fs_List, opts:OptionsPattern[]] := Function[{expr},
  With[{l = ComposeList[fs, expr]},
    If[OptionValue["verbose"], Print/@Transpose[{l, Prepend[fs, Identity]}]];
		Last[l]
  ]
]
composition[fs__, opts:OptionsPattern[]] := composition[{fs}]


cases[pattern_] := Union[Cases[#, pattern, Infinity]]&


End[]


EndPackage[]


Print[ToString@StringForm["[info]: '``' loaded", FileNameTake[$InputFileName, -3]]];
