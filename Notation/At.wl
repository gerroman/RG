BeginPackage["RG`Notation`At`", {"RG`Notation`Force`"}]


at::usage="at[expr, {var, a}] means expr at var = a \nat[expr, {var, a, b}]"


Begin["`Private`"]


at /: Format[at[expr_, {var_, a_}], TraditionalForm] := DisplayForm[
  SubscriptBox[""[expr], ToBoxes[var -> a, TraditionalForm]]
]
at /: Format[at[expr_, {var_, a_, b_}], TraditionalForm] := DisplayForm[
  SubsuperscriptBox[
    ToBoxes[""[expr],TraditionalForm],
    ToBoxes[var -> a, TraditionalForm],
    ToBoxes[var -> b, TraditionalForm]
  ]
]

at /: Format[at[expr_, cond_], TraditionalForm] := DisplayForm[
  SubscriptBox[""[expr], ToBoxes[cond, TraditionalForm]]
]


(* ::Text:: *)
(*\:041f\:043e\:0434\:0441\:0442\:0430\:043d\:043e\:0432\:043a\:0430 \:043f\:0440\:0435\:0434\:0435\:043b\:043e\:0432*)


(* try substitution or evaluate a limit *)
force[at]=ReplaceAll[{
at[expr_,{var_,a_}]:>Quiet@Check[(expr/.var->a),Limit[expr,var->a]],
at[expr_,{var_,a_,b_}]:>Quiet[Check[(expr/.var->b),Limit[expr,var->b]]-Check[(expr/.var->a),Limit[expr,var->a]]]
}];


End[]


EndPackage[]


(* RG`Scripts`fileStamp[] *)
