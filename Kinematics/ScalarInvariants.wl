BeginPackage["RG`Kinematics`ScalarInvariants`", {
  "RG`Kinematics`ScalarProduct`"
}]


GetScalarInvariants::usage="GetScalarInvariants[{p1, \[Ellipsis], pn}] return a set of independent scalar invariants assuming energy-momenta conservation p1 + \[Ellipsis] + pn = 0"


Begin["`Private`"]


GetScalarInvariants[ps_List] := Module[{n = Length[ps], nInv, sps, invariants},
  nInv = n * (n - 1) / 2;
  sps = Flatten[Array[Map[(ScalarProduct[Total[#]])&, (Partition[Join[ps, ps[[;;(#-1)]]], #, 1])]&, n]];
  invariants = sps[[;;nInv]]
];
GetScalarInvariants[ps__] := GetScalarInvariants[{ps}]


End[]


EndPackage[]


Print[ToString@StringForm["[info]: `` loaded", $InputFileName]];
