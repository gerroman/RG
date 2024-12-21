(* ::Package:: *)

Needs["RG`Scripts`", "RG/Tools/Scripts.wl"];


Get["RG/Tools/Rules.wl"];


BeginPackage["RG`Tools`"]


hold::usage="hold[x] \[LongDash] replace {x -> Hold[x]}."

powerExpand::usage="powerExpand[x] \[LongDash] pull out x of powers assuming that x is positive."

factorIt::usage="factorIt[x] \[LongDash] factor out x from sums."

changeSign::usage="changeSign[x] \[LongDash] replace x^p -> (-x)^p (-1)^p"

groupIt::usage="groupIt[expr, func] \[LongDash] replace func[expr]->expr"

modify::usage="modify[pattern, func] \[LongDash] replace (expr:pattern) :> func[expr]"

pullIt::usage="pullIt[x] \[LongDash] pull x out of sums."

pullFactor::usage="pullFactor[x, func] \[LongDash] pull x out of func"

distribute::usage="distribute[outer, inner]"

eq::usage = "eq[expr, func] \[LongDash] form an equation HoldForm[expr] == func[expr]"

cases::usage = "cases[pattern] \[LongDash] just a shortcut for Union[Cases[#, pattern, Infinity]]&"

head::usage = "head[fname] \[LongDash] return first line of file contents";
sizeOf::usage = "sizeOf[expr] \[LongDash] evaluates number of leafs and size in bytes of 'expr'";

solve::usage="solve[expr, var] \[LongDash] solve expr w.r.t. var"


Begin["`Private`"];


hold[xs_List] := With[{rule=rule`hold[xs]},
  ReplaceRepeated[#, rule]&
];
hold[xs__]:=hold[{xs}]


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
    ReplaceAll[Thread[hms->-xs]]
]]
changeSign[xs__] := changeSign[{xs}]
changeSign[pattern_] := Function[expr, With[{xs=Union@Cases[expr, pattern, Infinity]},
  changeSign[xs][expr]
]]


groupIt[x_, func_:Expand] := With[{rule = rule`group[x, func]},
  ReplaceAll[#, rule]&
]
groupIt[xs_List, func_:Expand] := RightComposition@@(groupIt[#, func]&/@xs)


modify[pattern_, func_:Expand] := With[{rule = (ex:pattern) :> func[ex]},
  ReplaceAll[#, rule]&
]
modify[xs_List, func_:Expand] := With[{rule = (# -> func[#])& /@ xs},
  ReplaceAll[#, rule]&
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


pullFactor[pattern_, func_] := With[{rule = rule`pullFactor[pattern, func]},
  ReplaceRepeated[#, rule]&
]


distribute[outer_, inner_:Plus] := With[{rule = rule`distribute[outer, inner]},
  ReplaceRepeated[#, rule]&
]


head[fname_String] := Module[{stream, result = $Failed},
  If[FileExistsQ[fname],
    (
      log[StringForm["\"``\" (`` bytes)", fname, FileByteCount[fname]], "prefix"->"[file]: "];
      stream = OpenRead[fname];
      result = ReadLine[stream];
      Close[stream];
    ),
    log[StringForm["can not find '``'", fname], "prefix"->"[error]: "];
  ];
  Return[result];
];


head[fname_String, n_Integer] := Module[{stream, result = $Failed},
  If[FileExistsQ[fname],
    (
      log[StringForm["\"``\" (`` bytes)", fname, FileByteCount[fname]], "prefix"->"[file]: "];
      stream = OpenRead[fname];
      result = StringRiffle[
        Table[ReadLine[stream], n] // DeleteCases[EndOfFile],
        {"", "\n", ""}
      ];
      Close[stream];
    ),
    log[StringForm["can not find '``'", fname], "prefix" -> "[error]: "];
  ];
  Return[result];
];


sizeOf[expr_] := Module[
  {leafs = LeafCount[expr], bytes = Quantity[ByteCount[expr], "Bytes"]},
  bytes = 1`3 * Which[
    QuantityMagnitude[bytes] > 10^9, UnitConvert[bytes, "Gigabytes"],
    QuantityMagnitude[bytes] > 10^6, UnitConvert[bytes, "Megabytes"],
    QuantityMagnitude[bytes] > 10^3, UnitConvert[bytes, "Kilobytes"],
    True, bytes
  ];
  {leafs, bytes}
];


solve[expr_List, vars_List] := Module[{
    vs=Array[v, Length[vars]]
  },
  Solve[expr //. Thread[vars->vs], vs] //. Thread[vs->vars]
]

solve[expr_, var_] := solve[{expr}, {var}]


End[]


EndPackage[]


Global`forceFlag = RG`Scripts`argparse["force", False];
SetOptions[RG`Scripts`Export, "force"->Global`forceFlag];


RG`Scripts`systemStamp[];
RG`Scripts`timeStamp[];
RG`Scripts`log[StringForm["working directory: '``'", Directory[]]];
RG`Scripts`fileStamp[];
