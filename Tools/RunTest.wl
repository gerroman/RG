RunTest::usage = "RunTest[fname] -- run tests (VerificationTest[]) in 'fname'"


RunTest::help = "
DESCRIPTION:
  fname -- filename string

EXAMPLES:
  math -script RunTest.wl Tests.wlt
  math -script RunTest.wl *.wl
";


Needs["RG`Scripts`", "RG/Tools/Scripts.wl"]


main[] := Module[{argc, argv, result, fname, report, succeeded, failed, time, memory},
  {argc, argv} = RG`Scripts`argparse[];
  If[argc == 0, Return[Null]];
  If[(argc == 2 && argv[[2]] == "-h") || (argc == 1),
    Write["stderr", RunTest::usage];
    Write["stderr", RunTest::help];
    Exit[0];
  ];
  fname = argv[[2]];
  report = TestReport[fname];
  succeeded = report["TestsSucceededCount"];
  failed = report["TestsFailedCount"];
  time = 1`6 *report["TimeElapsed"];
  memory = UnitConvert[1`6 * Total[Map[#["MemoryUsed"]&, report["TestResults"]]], "Megabytes"];
  result = ToString@StringForm["[``]: Succeed: ``, Failed: ``, TimeElapsed: ``, MemoryUsed: ``",
    FileNameTake[fname, -1],
    succeeded,
    failed,
    time,
    memory
  ];
  Write["stdout", result];
  If[failed > 0, Function[idx, Write["stderr",
      ToString[StringForm["[Test #``]: `` == `` != ``", idx,
        Sequence@@(ToString[report["TestResults"][[idx,1,#]], InputForm]& /@
          {"Input","ActualOutput","ExpectedOutput"}
        )
      ]]
    ]] /@ report["TestsFailedIndices"]
  ];
];


main[];
