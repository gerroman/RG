RunCheck::usage = "RunCheck[fname] -- check syntax in file 'fname'"


RunCheck::help = "
DESCRIPTION:
  fname -- filename string

EXAMPLES:
  math -script RunCheck.wl test.wl
  math -script RunCheck.wl *.wl
";


Needs["RG`SyntaxChecker`"];


parse[] := Which[
  $ScriptCommandLine =!= {}, {Length[#], #}&[$ScriptCommandLine],
  Length[$CommandLine] >= 2 && $CommandLine[[2]] === "-script", {Length[#], #}&[$CommandLine[[3;;]]],
  True, {0, {}}
];


main[] := Module[{argc, argv, result},
  {argc, argv} = parse[];
  If[argc == 0, Return[Null]];

  If[(argc == 2 && argv[[2]] == "-h") || (argc == 1),
    Write["stderr", RunCheck::usage];
    Write["stderr", RunCheck::help];
    Exit[0];
  ];

  result = And@@(syntaxChecker /@ Rest[argv]);
  Exit[If[result, 0, 1]];
];


main[];
