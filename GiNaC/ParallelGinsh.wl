(* ::Package:: *)

(* It is possible to use $ginsh = {"wsl", "ginsh"}; to run ginsh under WSL *)
If[Not@ValueQ[$ginsh], 
  $ginsh = Block[
    {test = Function[{value}, Quiet@RunProcess[value, "ExitCode", "exit();"] === 0]},
    Which[
	  test["ginsh"], "ginsh",
	  test[{"wsl", "ginsh"}], {"wsl", "ginsh"},
	  True, "ginsh"
    ]
  ]
]


ginshN::error = "something wrong ... :\n-------error-------\n``----end of error----";
ginshN[expr_, precision_Integer, batchSize_Integer:100] := Module[{
       prog,
       Gs = DeleteDuplicates[Cases[expr, _G | _H | _MZV, All]],
       GsN
     },
    (*PrintTemporary["Length Gs = ", Length[Gs]];*)
    If[Length[Gs] == 0, Return[expr]];
    GsN = If[batchSize > 0,
       (*PrintTemporary[StringForm["using `` parallel kernels ... ", $KernelCount]];*)
       Flatten[ParallelMap[
          ginshListN[#, precision] &, 
          Partition[Gs, batchSize, batchSize, 1, {}], 
          {1}
        ]]
        , 
        ginshListN[Gs, precision]
      ];
    expr /. Dispatch[GsN]
];


ginshListN[exprList : {(_G | _H | _MZV) ..}, precision_Integer] := 
   Module[{
     prog,
     GsN,
     toGinsh = Replace[{
         gh : (_G | _H) :> ToString[gh, InputForm],
         mzv_MZV :> 
         "zeta(" <> ToString[Abs[List @@ mzv], InputForm] <> "," <> 
          ToString[Sign[List @@ mzv], InputForm] <> ")"
        }],
     evalFunction = 
      "evalf(" <> 
        StringReplace[#1, {" " -> "", "Sqrt" -> "sqrt", "[" -> "(", 
      "]" -> ")"}] <> ");\n" &,
     output,
     stream,
     fname
     },
    prog = StringJoin[
        ToString@StringForm["Digits=``:\n", Ceiling[precision]],
        StringJoin @@ evalFunction /@ toGinsh /@ exprList,
        "exit();\n"
      ];
    stream = OpenWrite[BinaryFormat->True];(* use temporary file for ginsh program *)
	WriteString[stream, prog];
    fname = Close[stream];
	output = If[((Head[$ginsh] === List) && ($ginsh[[1]] === "wsl")),
	  (* using ginsh under WSL *)
	  RunProcess[Flatten@{$ginsh, StringReplace[fname, {"C:\\"->"/mnt/c/", "\\"->"/"}]}, All]
	  ,
	  RunProcess[Flatten@{$ginsh, fname}, All]
	];
    DeleteFile[fname];
    If[output["StandardError"] != "",
      Message[ginshN::error, output["StandardError"]];
      Return[Thread[exprList -> exprList]]
     ];
    GsN = Check[ToExpression /@ StringSplit[
          StringReplace[
             output["StandardOutput"],
             {a : NumberString ~~ "E" ~~ b : NumberString :> 
           a <> "*^" <> b}
           ]
        ], $Failed];
    If[Length[GsN] != Length[exprList],
       Message[ginshN::error, "Length[Gs] != Length[GsN]\n"];
       Return[Thread[exprList -> exprList]]
     ];
    Thread[exprList -> N[GsN, precision]]
];


Quiet@If[
 RunProcess[$ginsh, "ExitCode", "exit();"] =!= 0,
 Print[Style["ginsh not found. Visit https://ginac.de/ to install GinaC.", {Bold, Red}]]
]
