BeginPackage["RG`Scripts`", {"RG`Tools`"}];


Begin["`Private`"];


Off[FrontEndObject::notavail];
On[Assert];


systemStamp[];
timeStamp[];
note[Directory[]];


Get["RG/Tools/SetDrawOptions.wl"];


forceFlag = argparse["force", False];
SetOptions[export, "force":>forceFlag];
SetOptions[exportFigure, "force":>forceFlag];


quietFlag = argparse["quiet", False];
verboseFlag = argparse["verbose", False];
If[quietFlag, verboseFlag = False];


SetOptions[load, "verbose" :> verboseFlag];
SetOptions[loadFigure, "verbose" :> verboseFlag];


Unprotect[Print];
Print[expr__] := WriteString["stderr", expr];
Protect[Print];


End[];


EndPackage[];
