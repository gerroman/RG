BeginPackage["RG`GiNaC`"]


G::usage = "G[{z1, ..., zn}, y] evaluate iterative polylog";
EvalG::usage = "EvalG[{Re[z1], ..., Re[zn]}, {Im[z1], .., Im[zn]}, y] -- for Complex {z1, ..., zn} and Real y";


Begin["`Private`"];


Install[FileNameJoin[{"RG", "GiNaC", "G.bin"}]];


G[zs_List, y_Real] := Complex @@ EvalG[Re[zs], Im[zs], y];


End[]


EndPackage[]
