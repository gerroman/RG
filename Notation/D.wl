BeginPackage["RG`Notation`D`", {"RG`Notation`Force`"}]


d::usage="d[expr] represent Dt[expr];d[expr, var] represent D[expr, var]"
dt::usage="dt[expr] represent Dt[expr];d[expr, var] represent Dt[expr, var]"


Begin["`Private`"]


d/:Format[d[arg_], TraditionalForm] := HoldForm[Dt[arg]];
d/:Format[d[args__], TraditionalForm] := HoldForm[D[args]];
d/:Format[d[{l_, m_}], TraditionalForm] := DisplayForm[RowBox[{SuperscriptBox["d", ToBoxes[m, TraditionalForm]], ToBoxes[l, TraditionalForm]}]]
d/:Format[d[{l_, m_}, "factor" -> factor_], TraditionalForm] := With[{
  num = If[Numerator[factor] =!= 1, ToBoxes[Numerator[factor], TraditionalForm], Nothing],
  den = Denominator[factor],
  db = If[m =!= 1, SuperscriptBox["d", ToBoxes[m, TraditionalForm]], ToBoxes["d"]]
  },
  DisplayForm[
  If[den === 1,
    RowBox[{num, db, ToBoxes[l, TraditionalForm]}]
    ,
    FractionBox[
      RowBox[{num, db, ToBoxes[l, TraditionalForm]}],
      ToBoxes[den, TraditionalForm]
    ]
  ]]
]

dt/:Format[dt[arg_], TraditionalForm] := HoldForm[Dt[arg]];
dt/:Format[dt[arg_, opts:OptionsPattern[]], TraditionalForm] := HoldForm[Dt[arg]];


d/:force[d, opts:OptionsPattern[]] := ReplaceAll[#, d -> (D[##, opts]&)]&
dt/:force[dt, opts:OptionsPattern[]] := ReplaceAll[#, dt -> (Dt[##, opts]&)]&





End[]


EndPackage[]


(* RG`Scripts`fileStamp[] *)
  
