Begin["rule`"]


hold::usage = "rule`hold[x] \[LongDash] rules to replace {x -> Hold[x]}"

release::usage = "rule`release[x]  \[LongDash] rules to replace {(Hold|HoldForm)[x]->x}"

powerExpand::usage = "rule`powerExpand[x] \[LongDash] rule to pull x out of Power[] and Abs[]"

factor::usage = "rule`factor[x] \[LongDash] rule to factor x out of Plus[]"

pull::usage = "rule`pull[x] \[LongDash] rule to pull x out of Plus[]"

group::usage = "rule`group[x, func] \[LongDash] rule to replace {func[x] -> x}"


Begin["`Private`"]


rule`hold[xs_List] := Join[
  {expr_Hold :> expr, expr_HoldForm :> expr},
  Thread[xs -> Hold/@xs]
]
rule`hold[xs__] := rule`hold[{xs}]


rule`release[xs_List] := Flatten[{Hold[#]->#, HoldForm[#]->#}& /@ xs]
rule`release[xs__] := rule`release[{xs}]


rule`powerExpand[xs_List] := Flatten[{
  expr_Hold :> expr,
  expr_HoldForm :> expr,
  (#^p_)^q_ :> #^Expand[(p q)],
  (expr_ * #^p_.)^q_ :> expr^q * #^(p q),
  Abs[expr_. * #^p_.] :> Abs[expr] * #^p
}& /@ xs]
rule`powerExpand[xs__] := rule`powerExpand[{xs}]


rule`factor[x_] := {
  expr_Hold :> expr,
  expr_HoldForm :> expr,
  Plus[expr:(x * _.), other:(x * _.)..] :> x Plus@@({expr, other}/x)
}


rule`pull[x_] := {
  expr_Hold :> expr,
  expr_HoldForm :> expr,
  Plus[x, others__] :> x (1 + Plus@@({others}/x)),
  Plus[expr:(x * _), other__] :> x Plus@@({expr, other}/x)
}


rule`group[x_, func_:Expand] := func[x] ->x


End[]


End[]


Print[ToString@StringForm["[info]: '``' loaded", FileNameTake[$InputFileName, -3]]];
