(* ::Package:: *)


rule`hold::usage = "rule`hold[x] \[LongDash] get rules to replace {x -> Hold[x]}"

rule`release::usage = "rule`release[x]  \[LongDash] get rules to replace {(Hold|HoldForm)[x]->x}"

rule`powerExpand::usage = "rule`powerExpand \[LongDash] rule to pull Hold[x] out of powers"

rule`factor::usage = "rule`factor \[LongDash] rule to factor Hold[x] out of sums"

rule`pull::usage = "rule`pull \[LongDash] rule to pull Hold[x] out of sums "

(* ::Section:: *)
(*Rules*)


Begin["rule`Private`"]


rule`hold[{x_/;NumberQ[x]}] := {
	(expr_Hold)^(p_.) :> expr^p,
	x -> Hold[x],
	-x -> -Hold[x],
	(i_ /; NumberQ[i] && IntegerQ[Log[x, i]]) :> Hold[x]^Log[x, i],
	(i_ /; NumberQ[i] && IntegerQ[Log[x, -i]]) :> (-1) * Hold[x]^Log[x, (-i)]
}

rule`hold[xs_List] := Join[
  {expr_Hold:>expr},
	Thread /@ {xs->(Hold/@xs), Expand[-xs]->-(Hold/@xs)} //
	  Transpose //
	  Flatten
];

rule`hold[xs__] := rule`hold[{xs}]


rule`release[xs_List] := (Hold|HoldForm)[#]-># & /@ xs;


rule`powerExpand = {
	Times[expr_,  factor:(_Hold^_.)..]^p_ :> PowerExpand[Times[factor]^p] expr^p,
	Times[factor:(_Hold^_.)..]^p_ :> PowerExpand[Times[factor]^p],
	Abs[Times[expr_,  factor:(_Hold^_.)..]] :> Times[factor] Abs[expr],
	Abs[Times[factor:(_Hold^_.)..]] :> Times[factor]
}


rule`factor = (
	Plus[expr:((factor:(x_Hold)^(_.)) * _.), rest:((x_Hold)^(_.) * _.).., others___] :> 
    Plus[factor * Plus@@({expr, rest} / factor), others]
)


rule`pull = (
  Plus[expr:((term:(_Hold)^(p_. /; (p > 0))) * _.).., others__] :> 
    Plus[term * Plus@@({expr, others} / term)] 
)


End[]


(* ::Section:: *)
(*Tools*)


BeginPackage["RG`Tools`"]


hold::usage="hold[x] \[LongDash] replace {x -> Hold[x]}"

release::usage="release[x] \[LongDash] replace {(Hold|HoldForm)[x]->x}"

powerExpand::usage="powerExpand[x] \[LongDash] pull out x of powers assuming that x is positive.\n[note]: it is using hold[]/release[] pair"

factorIt::usage="factorIt[x] \[LongDash] factor out x from sums.\n[note]: it is using hold[]/release[] pair"

changeSign::usage="changeSign[x] \[LongDash] replace x^p -> (-x)^p (-1)^p"

groupIt::usage="groupIt[expr, func] \[LongDash] replace func[expr]->expr"

modify::usage="modify[pattern, func] \[LongDash] replace (expr:pattern) :> func[expr]"

pullIt::usage="pullIt[x] \[LongDash] pull x out of sums.\n[note]: it is using hold[]/release[] pair"

eq::usage = "eq[expr, func] \[LongDash] form an equation HoldForm[expr] == func[expr]"

composition::usage = "composition[fs][expr] \[LongDash] sequentially apply all functions in the list `fs' to `expr' printing intermediate results (by default) and returning final expression\n[note]: it is equivalent to RightComposition[fs][expr]"

force::usage = "force[at | limit | sum | integrate | d] forces evaluation";

cases::usage = "cases[pattern] \[LongDash] just a shortcut for Union[Cases[#, pattern, Infinity]]&"


Begin["`Private`"];


hold[xs_List] := With[{rule=rule`hold[xs]},
	Function[expr, expr //. rule]
];
hold[xs__]:=hold[{xs}]


release[xs_List] := With[{rule=rule`release[xs]},
	Function[expr, expr //. rule]
]
release[xs__]:=release[{xs}]


powerExpand[xs_List] := Function[expr,
	expr //
		hold[xs] //
		ReplaceRepeated[#, rule`powerExpand]& //
		release[xs]
];
powerExpand[xs__] := powerExpand[{xs}]


factorIt[xs_List] := Function[expr,
	expr //
		hold[xs] //
		ReplaceRepeated[#, rule`factor]& //
		release[xs]
]
factorIt[xs__] := factorIt[{xs}]


changeSign[xs__] := powerExpand[-{xs}]


pullIt[xs_List] := Function[expr,
	expr //
		hold[xs] //
		ReplaceRepeated[#, rule`pull]& //
		release[xs]
]
pullIt[xs__] := pullIt[{xs}]



groupIt[ex_, func_:Expand] := With[{rule = func[ex] -> ex},
	Function[expr, expr /. rule]
]


modify[pattern_, func_:Expand] := With[{rule = (ex:pattern) :> func[ex]},
	Function[expr, expr /. rule]
]


SetAttributes[eq, HoldFirst]
Options[eq] = {Hold->True};
eq[expr_, fs_List:{Identity}, opts:OptionsPattern[]] := With[
	{
		func = RightComposition@@fs,
    lhs = If[OptionValue[Hold], Hold[expr], expr]
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


Options[composition] = {"verbose"->False}
composition[fs_List, opts:OptionsPattern[]] := Function[{expr},
  With[{l = ComposeList[fs, expr]}, 
    If[OptionValue["verbose"], Print/@Transpose[{Prepend[fs, Identity], l}]];
		Last[l]
  ]
]
composition[fs__, opts:OptionsPattern[]] := composition[{fs}]


cases[pattern_] := Union[Cases[#, pattern, Infinity]]&


End[]


EndPackage[]


Print[ToString@StringForm["[info]: '``' loaded", FileNameTake[$InputFileName, -3]]];
