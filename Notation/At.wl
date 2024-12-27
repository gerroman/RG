BeginPackage["RG`Notation`At`", {"RG`Notation`Force`"}]


at::usage="at[expr, {var, a}] means expr at var = a \nat[expr, {var, a, b}]"


Begin["`Private`"]


at /: Format[at[expr_, {var_, a_}], TraditionalForm] := DisplayForm[
  SubscriptBox[""[expr], ToBoxes[var == a, TraditionalForm]]
]
at /: Format[at[expr_, {var_, a_, b_}], TraditionalForm] := DisplayForm[
  SubsuperscriptBox[
    ToBoxes[""[expr],TraditionalForm],
    ToBoxes[var == a, TraditionalForm],
    ToBoxes[var == b, TraditionalForm]
  ]
]

at /: Format[at[expr_, cond_], TraditionalForm] := DisplayForm[
  SubscriptBox[""[expr], ToBoxes[cond, TraditionalForm]]
]


End[]


EndPackage[]


RG`Scripts`fileStamp[]
