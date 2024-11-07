Needs["RG`Scripts`", "RG/Tools/Scripts.wl"];

Get["RG/Tools/Rules.wl"];
Get["RG/Tools/SetDrawOptions.wl"];


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

force::usage = "force[at | limit | sum | integrate | d] forces evaluation";

cases::usage = "cases[pattern] \[LongDash] just a shortcut for Union[Cases[#, pattern, Infinity]]&"


Begin["`Private`"];


hold[xs_List] := With[{rule=rule`hold[xs]},	ReplaceRepeated[#, rule]&];
hold[xs__]:=hold[{xs}]


release[xs_List] := With[{rule=rule`release[xs]},	ReplaceRepeated[#, rule]&]
release[xs__]:=release[{xs}]


powerExpand[x_] := With[{rule=rule`powerExpand[x]}, ReplaceRepeated[#, rule]&]
powerExpand[xs_List] := RightComposition@@(powerExpand/@xs);
powerExpand[xs__] := powerExpand[{xs}]


factorIt[x_] := With[{rule=rule`factor[x]}, ReplaceRepeated[#, rule]&]
factorIt[xs_List] := RightComposition@@(factorIt/@xs)
factorIt[xs__] := factorIt[{xs}]


pullIt[x_] := With[{rule=rule`pull[x]}, ReplaceRepeated[#, rule]&]
pullIt[xs_List] := RightComposition@@(pullIt/@xs)
pullIt[xs__] := pullIt[{xs}]


changeSign[xs_List] := With[{hs=Hold/@xs, hms=Hold/@(-xs)}, Function[expr, 
  expr //
	hold[xs] //
	ReplaceAll[Thread[hs -> (-1)*hms]] //
	release[-xs]
]]
changeSign[xs__] := changeSign[{xs}]


groupIt[x_, func_:Expand] := With[{rule = rule`group[x, func]},
  ReplaceRepeated[#, rule]&
]
groupIt[xs_List, func_:Expand] := With[{rule = rule`group[#, func]& /@ xs},
  ReplaceRepeated[#, rule]&
]


modify[pattern_, func_:Expand] := With[{rule = (ex:pattern) :> func[ex]},
  ReplaceRepeated[#, rule]&
]
modify[xs_List, func_:Expand] := With[{rule = (# -> func[#])& /@ xs},
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


cases[pattern_] := Union[Cases[#, pattern, Infinity]]&


End[]


EndPackage[]


Print[ToString@StringForm["[info]: '``' loaded", FileNameTake[$InputFileName, -3]]];

