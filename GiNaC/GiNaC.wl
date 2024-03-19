BeginPackage["RG`GiNaC`"]


Global`G::usage = "G[{z1, ..., zn}, y] evaluate iterative polylog";
EvalG::usage = "EvalG[{Re[z1], ..., Re[zn]}, {Im[z1], .., Im[zn]}, y] -- for Complex {z1, ..., zn} and Real y";


Begin["`Private`"];


Install[FileNameJoin[{"RG", "GiNaC", "bin" "G.bin"}]];


Global`G /: N[Global`G[zs_List, y_]] := Complex @@ EvalG[N@Re[zs], N@Im[zs], N@y];


End[]


EndPackage[]
