(* ::Package:: *)


ExtractCode::usage = "ExtractCode[ifname, output, \"force\"->(True|False)] -- extract \
input and code cells from the notebook and save it to separate code file. Option \"force\" \
forces overwrite of the file"


ExtractCode::help = "
DESCRIPTION:
  ifname -- notebook file name (String)
  output -- output file name (default = Null means save notebook contents to the file with changed \
extension to wl)
EXAMPLES:
  math -script ExtractCode.wl notebook.nb code.wl
  math -script ExtractCode.wl notebook.nb
  math -script ExtractCode.wl notebook.nb -force
"

ExtractCode::exist = "file `` does exist, use '-force' flag to overwrite"
Options[ExtractCode] = {"force"->False};
ExtractCode[ifname_, output:(Null|_String), opts:OptionsPattern[]] := Module[{
  data,
  ofname = output,
  force = OptionValue["force"]
},
If[Not@FileExistsQ[ifname], (
  Message[General::notfound, ifname];
  Return[$Failed];
)];
ofname = If[output === Null, FileBaseName[ifname]<>".wl", output];
If[FileExistsQ[ofname] && Not[force], (
  Message[ExtractCode::exist, ofname];
  Return[$Failed]
)];
Print[ifname, " => ", ofname];
UsingFrontEnd[
With[{nb = NotebookOpen[ifname,Visible->False]}, (
  data = Riffle[NotebookImport[nb, "Input"|"Code"->"InputText"],"\n\n"];
  NotebookClose[nb];
)]
];
With[{fstream=OpenWrite[ofname]}, (
  WriteString[fstream,#]&/@data;
  Close[fstream];
)];
Return[ofname];
]


parse[] := Which[
  $ScriptCommandLine =!= {},
  {Length[#], #}&[$ScriptCommandLine],
  Length[$CommandLine] >= 2 && $CommandLine[[2]] === "-script",
  {Length[#], #}&[$CommandLine[[3;;]]],
  True, {0, {}}
];


main[] := Module[{argc, argv, result, forceFlag},
  {argc, argv} = parse[];

  If[argc == 0,
    (*not running as a script*)
    Return[Null]
  ];

  If[MemberQ[argv, "-h"] || MemberQ[argv, "-help"] || (argc == 1),
    Write["stderr", ExtractCode::usage];
    Write["stderr", ExtractCode::help];
    Exit[0];
  ];

  forceFlag = MemberQ[argv, "-force"];
  If[forceFlag, (
    argv = DeleteCases[argv, "-force"];
    argc -= 1;
  )]

  Print[
    ToString@StringForm["[info]: extracting code from `` ... ", Rest[argv]]
  ];

  result = ExtractCode[#, Null, "force"->forceFlag]& /@ Rest[argv];

  Exit[Boole[MemberQ[result, $Failed]]];
];


main[]
