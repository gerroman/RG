check::usage = "check[fname] -- check syntax in file 'fname'"

check::help = "
DESCRIPTION:
  fname -- filename string

EXAMPLES:
  math -script check.wl test.wl
  math -script check.wl *.wl
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
    Write["stderr", check::usage];
    Write["stderr", check::help];
    Exit[0];
  ];

  result = And@@(syntaxChecker /@ Rest[argv]);
  Exit[If[result, 0, 1]];
];

main[];
