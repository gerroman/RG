#!/usr/bin/env -S WolframScript -sl -script

run[fname_] := (
  Print["[Running] tests from ", fname];
  report = TestReport[fname];
  succeed = report["TestsSucceededCount"];
  failed = report["TestsFailedCount"];
  If[failed == 0,
    Print[ToString@StringForm["[Success] for `` tests", succeed]],
    (
      Print[ToString@StringForm["[Failed] for `` tests of ``", failed, failed + succeed]];
      Throw[{fname, report["TestsFailedWrongResults"] // First}];
    )
  ];
);


result = Catch[Scan[run, Rest[$ScriptCommandLine]]];
If[result === Null,
  Print["[Success]: all tests passed"],
  (
    Print["[Failed]: first failed test in the file ", result[[1]]];
    Print["\n[Test]: #", result[[2]]["TestIndex"]];
    Print[];
    Print["Actual output  :\n", ToString[result[[2]]["ActualOutput"]]];
    Print[];
    Print["Expected output:\n", ToString[result[[2]]["ExpectedOutput"]]];
    Print[];
    Print["Actual output (InputForm):\n", InputForm[result[[2]]["ActualOutput"]]];
  )
];