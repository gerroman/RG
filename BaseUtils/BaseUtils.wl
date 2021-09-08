(* ::Package:: *)

BeginPackage["RG`BaseUtils`"];


assert::usage = "
  assert[expr] evaluate expr with temporally enabled assertions
";


carryFirst::usage = "
  carryFirst[func][args] return function f[##, args] &
";


carryLast::usage = "
  carryLast[func][args] return function f[args, ##] &
";


load::usage = "
  load[file, symbol, definitions] load from file (if it exists) or
  execute symbol's definitions and save it to file
";


reload::usage = "
  reload[context] remove definitions context`* and reload it
";


temporary\[LetterSpace]directory::usage = "
  temporary\[LetterSpace]directory return location to save auxiliary files
";


verbose::usage = "
  verbose -> True option to make function more verbose
"


Begin["`Private`"];


SetAttributes[assert, HoldAll];
assert[expr__] := (
  On[Assert];
  Assert[expr];
  Off[Assert];
);


Options[reload] = {"verbose" -> False};

reloadingMsg = "context: ";
removingMsg = "list of symbols to remove: ";
loadedMsg = "list of loaded symbols: ";

reload[context_String, opts:OptionsPattern[]] := With[{
    form = StringJoin[{context, "*"}],
    verbose = OptionValue[reload, "verbose"]
  },
  If[verbose, Print[reloadingMsg, context]];
  If[NameQ[form],
    If[verbose,  Print[removingMsg, Names[form]]];
    Unprotect[form];
    Remove[form];
  ];
  Get[context];
  If[verbose, Print[loadedMsg, Names[form]]];
];


carryFirst[func_Symbol] := Function[expr, func[expr, ##]] &;
carryLast[func_Symbol] := Function[expr, func[##, expr]] &;


temporary\[LetterSpace]directory = FileNameJoin[{$TemporaryDirectory, "RG"}];
If[
  (! FileExistsQ[temporary\[LetterSpace]directory]),
  CreateDirectory[temporary\[LetterSpace]directory]
];


SetAttributes[load, HoldAll];
load::get = "Getting `1` ...";
load::save = "Saving `1` ...";
load::failed = "Error: failed to load file `1`";
load[fname_, symbol_, definition_] := With[
  {path = FileNameJoin[{temporary\[LetterSpace]directory, fname}]},
  If[FileExistsQ[path], (
      PrintTemporary[ToString[StringForm[load::get, path]]];
      Get[path]
    ),(
      definition;
      PrintTemporary[ToString[StringForm[load::save, path]]];
      DumpSave[path, symbol];
    )
  ]
];

load[fname_] := With[
  {path = FileNameJoin[{temporary\[LetterSpace]directory, fname}]},
  If[FileExistsQ[path],
    (PrintTemporary@StringForm[load::get, path]; Get[path]),
    PrintTemporary@StringForm[load::failed, path]
  ]
]


Protect[verbose]


End[];


EndPackage[];

