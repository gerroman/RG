BeginPackage["RG`Kinematics`ConservationRules`"]


GetConservationRules::usage = "GetMomentumConservationRules[{p1, \[Ellipsis], pn}] return rules to substitute sums of external momenta assuming p1 + \[Ellipsis] + pn = 0"


Begin["`Private`"]


GetConservationRules[ps_List] := Module[{subsets, rules, n},
  n = Length[ps];
  subsets = Subsets[ps, {If[EvenQ[n], n/2 + 1, Ceiling[n/2]], Length[ms]}];
  rules = (Total[#] -> (-1) * Total[Complement[ps, #]])& /@ subsets;
  Join[rules, MapAt[-#&, rules, {All, All}]]
]

GetConservationRules[eq_Equal] := With[{
    ps = Flatten[{1, -1}*List@@@List@@eq] // DeleteCases[0]
  },
  GetConservationRules[ps]
]


GetConservationRules[p_, ps__] := GetConservationRules[{p, ps}];


End[]


EndPackage[]


Print[ToString@StringForm["[info]: `` loaded", $InputFileName]];
