(* ::Package:: *)


BeginPackage["RG`BaseUtils`"];


assert::usage = "
  assert[expr] evaluate expr with temporally enabled assertions
";


reload::usage = "
  reload[context] remove definitions context`* and reload it
";


modify::usage = "
  modify[pattern, fs] create function to replace all matches of the pattern to results of consequent application of functions fs to these matches
  modify[{x1, ...}, fs] create function for specific x1, ...
";


OverTilde::usage = "
  OverTilde[func][args][expr] works as func[expr, args] i.e. it creates operator OverTilde[func][args] for the first argument of func
";


Begin["`Private`"];


SetAttributes[assert, HoldAll];
assert[expr__] := (
  On[Assert];
  Assert[expr];
  Off[Assert];
);


Options[reload] = {"verbose" -> False};
reload[context_String, opts:OptionsPattern[]] := With[{
    form = StringJoin[{context, "*"}],
    verbose = OptionValue[reload, "verbose"],
    reloadingMsg = "context: ",
    removingMsg = "list of symbols to remove: ",
    loadedMsg = "list of loaded symbols: "
  },
  If[verbose, Print[reloadingMsg, context]]
  If[NameQ[form],
    If[verbose,  Print[removingMsg, Names[form]]];
    Unprotect[form];
    Remove[form];
  ];
  Get[context];
  If[verbose, Print[loadedMsg, Names[form]]];
];


modify[xs_List, fs_List] := With[
   {rules = Thread[Rule[xs, Map[RightComposition@@fs, xs]]]},
   ReplaceAll[rules]
];
modify[pattern_, fs_List] := Function[expr,
  With[{xs = Union@Cases[{expr}, pattern, Infinity]},
    modify[xs, fs][expr]
  ]
];
modify[expr_, fs__] := modify[expr, {fs}]


OverTilde[func_Symbol] := Function[expr, func[expr, ##]] &;


End[];


EndPackage[];

