BeginPackage["RG`Notation`Sum`", {"RG`Notation`Force`"}]


RG`Notation`sum::usage="\
sum[expr, var] \[LongDash] indefinite sum w.r.t. var\n\
sum[expr, {var, i, j}] \[LongDash] definite sum w.r.t. var\n\
sum[expr1, expr2, ..., {}] \[LongDash] representation expr1 + expr2 + ... preserving order\
"


Begin["RG`Notation`Private`"]


RG`Notation`sum/:Format[RG`Notation`sum[expr_, var_], TraditionalForm] := DisplayForm[RowBox[{
  UnderscriptBox[ToBoxes[Style["\[Sum]",Larger], TraditionalForm], ToBoxes[var,TraditionalForm]]
  ,
  ToBoxes[expr, TraditionalForm]
}]];
RG`Notation`sum/:Format[RG`Notation`sum[expr_, {var_, i_, j_}], TraditionalForm] := DisplayForm[
  RowBox[{
    UnderoverscriptBox[
      ToBoxes["\[Sum]", TraditionalForm],
      ToBoxes[var == i, TraditionalForm],
      ToBoxes[j,TraditionalForm]
    ]
    ,
    ToBoxes[expr, TraditionalForm]
  }]
];

RG`Notation`sum/:Format[RG`Notation`sum[expr__, {}], TraditionalForm] := DisplayForm[RowBox[Riffle[
  ToBoxes[#, TraditionalForm]&/@{expr}
  ,
  "+"
]]]


RG`Notation`sum/:force[RG`Notation`sum, opts:OptionsPattern[]] := ReplaceAll[#, {
   RG`Notation`sum[a__, {}] :> Total[{a}],
   RG`Notation`sum -> (Sum[##, opts]&)
}]&;


End[]


EndPackage[]
