BeginPackage["RG`GiNaC`"]


G::usage = "G[{z1, ..., zn}, y] evaluate iterative polylog";


process = StartProcess[{"make", "-C", "/home/roman/Documents/Projects/RG/GiNaC", "link"}];
Print[ProcessStatus[process]];
Pause[1];
Print[ProcessStatus[process]];
Pause[1];
link = Install[LinkConnect["6464"]];
Print[link];


Begin["`Private`"];


G[zs_List, y_] := EvalG[Re[zs], Im[zs], y];


End[]


EndPackage[]
