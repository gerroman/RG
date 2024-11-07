RunTest::usage = "RunTest[fname] -- run tests (VerificationTest[]) in 'fname'"

RunTest::help = "
DESCRIPTION:
  fname -- filename string

EXAMPLES:
  math -script RunTest.wl Tests.wlt
  math -script RunTest.wl *.wl
";


Needs["RG`Scripts`", "RG/Tools/Scripts.wl"]


main[] := Module[{argc, argv, result},
  {argc, argv} = RG`Scripts`argparse[];
  If[argc == 0, Return[Null]];
  If[(argc == 2 && argv[[2]] == "-h") || (argc == 1),
    Write["stderr", RunTest::usage];
    Write["stderr", RunTest::help];
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
