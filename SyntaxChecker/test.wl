test::usage = "test[fname] -- run tests (VerificationTest[]) in 'fname'"

test::help = "
DESCRIPTION:
  fname -- filename string

EXAMPLES:
  math -script test.wl Tests.wlt
  math -script test.wl *.wl
";


parse[] := Which[
  $ScriptCommandLine =!= {}, {Length[#], #}&[$ScriptCommandLine],
  Length[$CommandLine] >= 2 && $CommandLine[[2]] === "-script", {Length[#], #}&[$CommandLine[[3;;]]],
  True, {0, {}}
];


main[] := Module[{argc, argv, result},
  {argc, argv} = parse[];
  If[argc == 0, Return[Null]];
  If[(argc == 2 && argv[[2]] == "-h") || (argc == 1),
    Write["stderr", test::usage];
    Write["stderr", test::help];
    Exit[0];
  ];
  result = ToString[
    StringForm["[``]:\tSucceed: ``,\tFailed: ``", 
      FileNameTake[#1, -1], 
      #2["TestsSucceededCount"], 
      #2["TestsFailedCount"]
    ]
  ]&;
  Scan[Write["stderr", result[#, TestReport[#]]]&, Rest[argv]];
];

main[];
