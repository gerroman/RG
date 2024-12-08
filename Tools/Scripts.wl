(* ::Package:: *)

Off[General::shdw];
BeginPackage["RG`Scripts`"];


(* ::Text:: *)
(*Logging utils*)
log::usage = "log[expr, verbose->True] \[LongDash] Print[expr],\nlog[expr, opts] converts 'expr' to string and call Write[] to stderr";
error::usage = "error[expr] \[LongDash] log  'expr' with '[ERROR]' prefix";
warning::usage = "warning[expr] \[LongDash] log  'expr' with '[warning]' prefix";
echo::usage = "echo[expr] \[LongDash] prints and return expr";


(* ::Text:: *)
(*System Information*)
timeStamp::usage = "timeStamp[] \[LongDash] print timeString[]"
systemStamp::usage = "systemStamp[] \[LongDash] print systemString[]"
fileStamp::usage = "fileStamp[] \[LongDash] print file loads"


(* ::Text:: *)
(*Update in system function*)
Export::usage=System`Export::usage;
Timing::usage=System`Timing::usage;


(* ::Text:: *)
(*Parsing command line arguments*)
argparse::usage = "argparse[] \[LongDash] returns {argc, argv}";


(* ::Text:: *)
(*Getting help*)
info::usage = "info[func] \[LongDash] get information about func: context, usage, attributes, options
info[func, All] \[LongDash] get full information about func including up/down values";


ansiwindows::usage="ansiwindows[expr, color]"


Begin["`Private`"];


(* ::Section:: *)
(* Logging *)


Options[log] = {
  "prefix" -> "[info]: ",
  "endl" -> "",
  "column" -> False,
  "verbose" -> False,
  "stream" -> "stderr",
  "width" -> 1000,
  "colorize" -> {
    "[info]" -> "\033[1;35m[info]\033[0m",
    "[directory]" -> "\033[1;35m[directory]\033[0m",
    "[....]" -> "\033[1;35m[....]\033[0m",
    "[usage]" -> "\033[1;37m[usage]\033[0m",
    "[load]" -> "\033[1;37m[load]\033[0m",
    "[file]" -> "\033[1;35m[file]\033[0m",
    "[hash]" -> "\033[1;35m[hash]\033[0m",
    "[export]" -> "\033[1;37m[export]\033[0m",
    "[ERROR]" -> "\033[1;31m[ERROR]\033[0m",
    "[time]" -> "\033[1;35m[time]\033[0m",
    "[date]" -> "\033[1;35m[date]\033[0m",
    "[seconds]" -> "\033[1;35m[seconds]\033[0m",
    "[test]" -> "\033[1;34m[test]\033[0m",
    "[OK]" -> "\033[1;32m[OK]\033[0m",
    "[note]" -> "\033[1;33m[note]\033[0m",
    "[echo]" -> "\033[1;33m[echo]\033[0m",
    "[running]" -> "\033[1;36m[running]\033[0m",
    "[exit]" -> "\033[1;36m[exit]\033[0m",
    "[RESULT]" -> "\033[1;31m[RESULT]\033[0m",
    "[init]" -> "\033[1;36m[init]\033[0m",
    "[warning]" -> "\033[0;33m[warning]\033[0m",
    "[args]" -> "\033[1;34m[args]\033[0m"
  }
};
log[expr_String, opts:OptionsPattern[]] := With[{
    message = StringJoin[OptionValue["prefix"], expr, OptionValue["endl"]]
  },
  If[OptionValue[verbose],
      Print[message]
    ,
    WriteString[
      OptionValue["stream"],
      StringJoin[
        "\r",
        StringReplace[message, OptionValue["colorize"]],
        "\n"
      ]
    ]
  ]
];
log[expr_StringForm, opts:OptionsPattern[]] := With[{
    message = StringJoin[OptionValue["prefix"], ToString@expr, OptionValue["endl"]]
  },
  If[OptionValue[verbose],
    Print[message]
    ,
    WriteString[
      OptionValue["stream"],
      StringJoin[
        "\r",
        StringReplace[message, OptionValue["colorize"]],
        "\n"
      ]
    ]
  ]
];
log[expr_List, opts:OptionsPattern[]] := If[OptionValue["column"],
  Scan[log[#, opts]&, expr]
  ,
  log[ToString[expr, FormatType->InputForm, TotalWidth->OptionValue["width"]], opts]
];

log[expr_, opts:OptionsPattern[]] := With[{
      message = StringJoin[
        OptionValue["prefix"],
        ToString[expr, FormatType->InputForm, TotalWidth->OptionValue["width"]],
        OptionValue["endl"]
      ]
    },
  If[OptionValue[verbose],
    Print[message]
    ,
    WriteString[
      OptionValue["stream"],
      StringJoin[
        "\r",
        StringReplace[message, OptionValue["colorize"]],
        "\n"
      ]
    ]
  ]
];
log[expr__, opts:OptionsPattern[]] := Scan[log[#, opts]&, {expr}];


error[expr_, opts:OptionsPattern[]] := log[expr, "prefix"->"[ERROR]: ", opts];
warning[expr_, opts:OptionsPattern[]] := log[expr, "prefix"->"[warning]: ", opts];
echo[expr_] := (log[expr, "width"->Infinity, "prefix"->"[echo]: "]; expr);


(* ::Section:: *)
(* System information *)


timeString := DateString[{"<", "Year", "-", "Month", "-", "Day", " ", "Hour",":", "Minute", ":", "Second", ">"}];
timeStamp[] := (
  log[timeString, "prefix" -> "[date]: "];
)


systemString := ToString@StringForm["``@`` : Wolfram Mathematica ``", $UserName, $MachineName, $Version];
systemStamp[] := (
  log[systemString];
)


fileString := ToString@StringForm["`` loaded", $InputFileName]
fileStamp[] := (
  log[fileString, "prefix" -> "[file]: "];
);


(* ::Section:: *)
(* Parsing command line arguments *)


argparse[] := Which[
  $ScriptCommandLine =!= {}, {Length[#], #}&[$ScriptCommandLine],
  Length[$CommandLine] >= 2 && $CommandLine[[2]] === "-script", {Length[#], #}&[$CommandLine[[3;;]]],
  True, {0, {}}
];


argparse[name_String, False] := Module[{argc, argv, value},
  {argc, argv} = argparse[];
  value = MemberQ[argv, "-" <> name];
  If[value,
    log[StringForm["`` = `` (command line flag)", name, value], "prefix"->"[args]: "],
    log[StringForm["`` = `` (absent command line flag)", name, value], "prefix"->"[args]: "]
  ];
  Return[value]
];

argparse[name_String, default:(_Integer|_Real|_String)] := Module[
  {argc, argv, pos, value},
  {argc, argv} = argparse[];
  pos = Position[argv, "-" <> name];
  If[Length[pos] != 1 || pos[[1, 1]] + 1 > argc,
    log[StringForm["`` = `` (default value)", name, default], "prefix"->"[args]: "];
    Return[default];
  ];
  value = ToExpression[argv[[pos[[1, 1]] + 1]]];
  If[Head[value] =!= Head[default],
    log[StringForm["`` = `` (using default value)", name, default], "prefix"->"[args]: "];
    log[StringForm["'``' is not a '``'", argv[[pos[[1, 1]] + 1]], Head[default]], "prefix"->"[warning]: "];
    Return[default];
  ];
  log[StringForm["`` = `` (command line argument, default = ``)", name, value, default], "prefix"->"[args]: "];
  Return[value];
];


(* ::Section:: *)
(* Exporting results *)


RG`Scripts`Export::export = "saving '`1`' to the file '`2`' ... ";
RG`Scripts`Export::hashError = "hash differs ... ";
RG`Scripts`Export::hashSame = "hash is the same";


Options[RG`Scripts`Export] = Join[Options[System`Export], {
  "force" -> False,
  "Comments" -> ""
}];


getHash[fname_String, nMax_:10] := Module[{f, s, n=0, hash=$Failed},
  f = OpenRead[fname];
  While[((s = ReadLine[f]) =!= EndOfFile) && ((n += 1) < nMax),
    If[StringStartsQ[s, "(* [hash]: "],
      hash = ToExpression[StringTake[s, {11, -3}]];
      log[hash, "prefix"->"[hash]: "];
    ];
  ];
  Close[f];
  If[hash === $Failed,
    error["can not find [hash] comment"]
  ];
  Return[hash]
];


RG`Scripts`Export[
  fname_ /; StringMatchQ[FileExtension[ToString[fname]], {"m", "wl"}],
  expr_,
  opts:OptionsPattern[]
] := Module[
  {
    fnameFull = ToString[fname],
    force = OptionValue["force"],
    hash = Hash[expr],
    hashPrev = $Failed,
    comments = StringRiffle[{
        "[comments]: " <> ToString@OptionValue["Comments"],
        "[author]: " <> systemString,
        "[date]: " <> timeString,
        "[hash]: " <> ToString@Hash[expr]
      }, {"", " *)\n(* ", ""}
    ],
    exportOpts = FilterRules[{opts}, Options[System`Export]]
  },
  log[fnameFull];
  If[force || Not[FileExistsQ[fnameFull]],
    log[StringForm[RG`Scripts`Export::export, ToString[expr, TotalWidth->300],  fnameFull], "prefix"->"[export]: "];
    System`Export[fnameFull, expr, "Comments"->comments, Sequence@@exportOpts];
    log["complete", "prefix"->"[export]: "];
    Return[fnameFull];
  ];
  If[Not[force] && FileExistsQ[fnameFull],
    hashPrev = getHash[fnameFull];
    If[hashPrev === hash,
      log[RG`Scripts`Export::hashSame, "prefix"->"[hash]: "],
      (
        error[RG`Scripts`Export::hashError];
        Return[$Failed];
      )
    ];
  ];
  Return[fnameFull];
];
RG`Scripts`Export[
  fname_ /; StringMatchQ[FileExtension[ToString[fname]], {"png", "jpg"}],
  expr_,
  opts:OptionsPattern[{RG`Scripts`Export, System`Export, Graphics, Rasterize}]
] := Module[
  {
    fnameFull = ToString[fname],
    fnameHash = ToString[fname] <> ".hash",
    force = OptionValue["force"],
    hash = Hash[InputForm[expr]],
    comments = StringRiffle[{
        "[comments]: " <> ToString@OptionValue["Comments"],
        "[author]: " <> systemString,
        "[date]: " <> timeString
      }, {"", " *)\n(* ", ""}
    ],
    exportOpts = Complement[{opts}, FilterRules[{opts}, Options[RG`Scripts`Export]]]
  },
  log[fnameFull];
  If[force || Not@FileExistsQ[fnameFull] || Not@FileExistsQ[fnameHash],
    log[StringForm[RG`Scripts`Export::export, ToString[expr, TotalWidth->300],  fnameFull], "prefix"->"[export]: "];
    System`Export[fnameFull, expr, Sequence@@exportOpts];
    log["complete", "prefix"->"[export]: "];
    log[StringForm[RG`Scripts`Export::export, hash, fnameHash], "prefix"->"[export]: "];
    With[{f=OpenWrite[fnameHash]},
      WriteString[f, "(*"<>comments<>"*)\n"];
      Write[f, hash];
      Close[f];
    ];
    log["complete", "prefix"->"[export]: "];
    Return[fnameFull];
  ];
  If[hash === Get[fnameHash],
    log[RG`Scripts`Export::hashSame, "prefix"->"[hash]: "],
    (
      error[RG`Scripts`Export::hashError];
      Return[$Failed];
    )
  ];
  Return[fnameFull];
];
RG`Scripts`Export[args__] := System`Export[args];


(* ::Section:: *)
(* Timing *)


Options[RG`Scripts`Timing] = {"verbose"->False};
SetAttributes[RG`Scripts`Timing, HoldFirst];
RG`Scripts`Timing[expr_] := Module[{time, result},
  log[ToString[Unevaluated[expr]], "prefix"->"[time]: ", "endl"->" ... "];
  {time, result} = If[OptionValue[RG`Scripts`Timing, "verbose"],
    AbsoluteTiming[expr], Block[{Print}, AbsoluteTiming[expr]]
  ];
  log[ToString @ NumberForm[time, {6, 2}] <> " [seconds]", "prefix"->"[time]: "];
  Return[{time, result}];
];


(* ::Section:: *)
(* Information *)


emptyQ[expr_] := Length[expr] == 0;


SetAttributes[info, {HoldAll, Listable}];
info[expr_Symbol, None] := (
  log[
    If[ValueQ[expr::usage],
      expr::usage,
      ToString@StringForm["[warning]: `` is undefined", expr::usage]
    ],
    "width"->Infinity
  ];
);
info[expr_Symbol] := (
  log[Context[expr]];
  If[ValueQ[expr::usage], log[expr::usage, "prefix"->"[usage]: ", "width"->Infinity]];
  If[Not[emptyQ[Attributes[expr]]], log[Attributes[expr], "width"->Infinity]];
  If[Not[emptyQ[Options[expr]]], log[Options[expr], "column"->True, "prefix"->"[option]: "]];
);
info[expr_Symbol, All] := (
  info[expr];
  If[Not[emptyQ[UpValues[expr]]], log[UpValues[expr], "width"->Infinity]];
  If[Not[emptyQ[OwnValues[expr]]], log[OwnValues[expr], "width"->Infinity]];
  If[Not[emptyQ[DownValues[expr]]], log[DownValues[expr], "width"->Infinity]];
);
info[expr_String] := log[Names[expr], "width"->Infinity];


(* ::Text:: *)
(*Colorization in terminal on Windows*)
ansiwindows[str_String, color_:Gray] := With[{
  rgb = StringJoin[Riffle[ToString /@ Round[255 * List@@ColorConvert[color, RGBColor]], ";"]]
  },
  StringJoin[FromCharacterCode[27], "[38;2;", rgb, "m", str, FromCharacterCode[27], "[0m"]
]


End[];


EndPackage[];
On[General::shdw];


Off[FrontEndObject::notavail];

Global`forceFlag = argparse["force", False];
Global`verboseFlag = If[$OperatingSystem=="Windows", True, argparse["verbose", False]];

SetOptions[RG`Scripts`Export, "force"->Global`forceFlag];
SetOptions[log, "verbose"->Global`verboseFlag];
If[$OperatingSystem == "Windows",
  SetOptions[log, "colorize"->{(*
  "[info]" -> ansiwindows["[info]", Darker@Blue],
  "[date]" -> ansiwindows["[date]", Darker@Magenta],
  "[usage]" -> ansiwindows["[usage]", Darker@Yellow],
  "[ERROR]" -> ansiwindows["[ERROR]", Red],
  "[OK]" -> ansiwindows["[OK]", Lighter@Green],
  "[warning]" -> ansiwindows["[warning]", Lighter@Red],
  "[args]" -> ansiwindows["[args]", Lighter@Magenta],
  "[time]" -> ansiwindows["[time]", Darker@Cyan],
  "[seconds]" -> ansiwindows["[seconds]", Darker@Cyan],
  "[echo]" -> ansiwindows["[echo]", White],
  "[....]" -> ansiwindows["[....]", White],
  "[export]" -> ansiwindows["[export]", Magenta],
  "[hash]" -> ansiwindows["[hash]", Magenta],
  "[directory]" -> ansiwindows["directory"],
  "[load]"->ansiwindows["load"]
  *)}]
];

systemStamp[];
timeStamp[];
log[StringForm["working directory: '``'", Directory[]]];
fileStamp[];
