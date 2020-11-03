(* ::Package:: *)


BeginPackage["RG`BaseUtils`"];


assert::usage = "
  assert[expr] evaluate expr with temporally enabled assertions
";


reload::usage = "
  reload[context] remove definitions context`* and reload it
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


End[];


EndPackage[];

