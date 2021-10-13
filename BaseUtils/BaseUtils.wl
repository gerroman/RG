(* ::Package:: *)

BeginPackage["RG`BaseUtils`"];


assert::usage = "
  assert[expr] evaluate expr with temporally enabled assertions
";


carryFirst::usage = "
  carryFirst[func][args] return function func[##, args] &
";


carryLast::usage = "
  carryLast[func][args] return function func[args, ##] &
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
";


Begin["`Private`"];


Protect[verbose];


SetAttributes[assert, HoldAll];
assert::msg = "``";
Options[assert] = {verbose -> False};
assert[expr__, OptionsPattern[]] := (
  On[Assert];
  If[OptionValue[verbose], Message[assert::msg, HoldForm[expr]]];
  Assert[expr];
  Off[Assert];
);


Options[reload] = {verbose -> False};

reload::reloadingMsg = "context: ";
reload::removingMsg = "list of symbols to remove: ";
reload::loadedMsg = "list of loaded symbols: ";

reload[context_String, opts:OptionsPattern[]] := With[{
    form = StringJoin[{context, "*"}]
  },
  If[OptionValue[verbose], Message[reload::reloadingMsg, context]];
  If[NameQ[form],
    If[OptionValue[verbose],  Message[reload::removingMsg, Names[form]]];
    Unprotect[form];
    Remove[form];
  ];
  Get[context];
  If[OptionValue[verbose], Message[reload::loadedMsg, Names[form]]];
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


End[];


EndPackage[];