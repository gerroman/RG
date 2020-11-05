#!/usr/bin/env -S WolframScript -script

run[fname_] := (
  Print["[Running] tests from ", fname];
  report = TestReport[fname];
  succeed = report["TestsSucceededCount"];
  failed = report["TestsFailedCount"];
  If[failed == 0,
    Print[ToString@StringForm["[Success] for `` tests", succeed]],
    (
      Print[ToString@StringForm["[Failed] for `` tests of ``", failed, failed + succeed]];
      Throw[fname];
    )
  ];
);


result = Catch[Scan[run, Rest[$ScriptCommandLine]]];
If[result === Null,
  Print["[Success]: all tests passed"],
  Print["[Failed]: first failed test in the file ", result]
];