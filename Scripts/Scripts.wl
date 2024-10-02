BeginPackage["RG`Scripts`", {"RG`Tools`"}];


Begin["`Private`"];


Off[FrontEndObject::notavail];
On[Assert];


Get["RG/Tools/SetDrawOptions.wl"];


forceFlag = argparse["force", False];
SetOptions[export, "force"->forceFlag];
SetOptions[exportFigure, "force"->forceFlag];


verboseFlag = Not[argparse["quiet", False]];
verboseFlag = argparse["verbose", verboseFlag];
SetOptions[load, "verbose"-> verboseFlag];
SetOptions[loadFigure, "verbose"->verboseFlag];


If[Not[$BatchInput], setPostProcessing[True]];


Unprotect[Print];
Print[expr__] := WriteString["stderr", expr];
Protect[Print];


End[];


EndPackage[];
