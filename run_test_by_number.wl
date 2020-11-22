#!/usr/bin/env -S WolframScript -script

run[fname_, n_:Null] := Block[
  {
    result = Null
    , i
    , imax
  },

  Clear[test];
  Get[fname, Path -> {$InitialDirectory}];
  imax = Length[DownValues[test]];

  If[(n =!= Null),
    If[1 <= (i = ToExpression[n]) <= imax,
      (
        Print[ToString@StringForm["[Running]: test #`2` from `1`", fname, i]];
        Return[test[i]];
      )
      ,
      (
        Print[ToString@StringForm["[ Error ]: bad test number #`1` of `2` for `3`", n, imax, fname]];
        Return[Null];
      )
    ];
  ];

  result = Null;
  For[i = 1, i <= imax, i++, (
    Print[ToString@StringForm["[Running]: test #`2` of `3` from `1`", fname, i, imax]];
    result = test[i];
    If[result["Outcome"] == "Failed", Return[result]];
  )];

  Return[result];
];


Print["[ Start ]: ", DateString[]];
result = run @@ Rest[$ScriptCommandLine];

If[result === Null,
  Quit[255];
];

If[result["Outcome"] == "Success",
  (
    Print["[Success]"];
    Quit[0];
  )
  ,
  (
    Print["[ Error ]\n"];
    Print["Actual output  :\n", ToString@InputForm[result["ActualOutput"]]];
    Print[];
    Print["Expected output:\n", ToString@InputForm[result["ExpectedOutput"]]];
    Quit[255];
  )
];