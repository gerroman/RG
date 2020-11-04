(*
  wolfram -script test.wl
*)

run[fname_] := (
  Print["[Running]: tests from ", fname];
  report = TestReport[fname];
  succeed = report["TestsSucceededCount"];
  failed = report["TestsFailedCount"];
  If[failed == 0,
    Print[ToString@StringForm["[Success]: for `` tests", succeed]],
    Print[ToString@StringForm["[Failed]: for `` tests of ``", failed, failed + succeed]]
  ];
);

run["TestRG.wlt"];
run["./BaseUtils/TestBaseUtils.wlt"];
(* run["./FeynmanDiagrams/TestFeynmanDiagrams.wlt"]; *)
run["./Notation/TestNotation.wlt"];
run["./Calculation/TestCalculation.wlt"];
run["./Kinematics/TestKinematics.wlt"];