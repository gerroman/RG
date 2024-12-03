BeginPackage["RG`Kinematics`ScalarInvariants`"]


GetScalarInvariants::usage="GetScalarInvariants[{p1, \[Ellipsis], pn}, sp] return a set of independent scalar invariants assuming energy-momenta conservation p1 + \[Ellipsis] + pn = 0"


Begin["`Private`"]


GetScalarInvariants[ps_List, sp_:RG`Kinematics`ScalarProduct`ScalarProduct] := Module[{n = Length[ps], nInv, sps, invariants},
  nInv = n * (n - 1) / 2;
  sps = Flatten[Array[Map[With[{x = Total[#]}, sp[x, x]]&, (Partition[Join[ps, ps[[;;(#-1)]]], #, 1])]&, n]];
  invariants = sps[[;;nInv]]
];


End[]


EndPackage[]


Print[ToString@StringForm["[info]: `` loaded", $InputFileName]];
