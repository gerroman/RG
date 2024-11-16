RunScript::usage = "RunScript[fname] -- run 'fname' as script"


RunScript::help = "
DESCRIPTION:
  fname -- filename string

EXAMPLES:
  wolfram -script RunScript.wl script.wl
  wolfram -script RunScript.wl *.wl
";


SetOptions["stderr", "PageWidth"->Infinity];


Needs["RG`Scripts`", "RG/Tools/Scripts.wl"]


main[] := Module[{argc, argv, result},
  {argc, argv} = RG`Scripts`argparse[];
  If[argc == 0, Return[Null]];
  If[(argc == 2 && argv[[2]] == "-h") || (argc == 1),
    Write["stderr", RunScript::usage];
    Write["stderr", RunScript::help];
    Exit[0];
  ];
  Get[argv[[2]]];
  Exit[0];
];


main[];
