(* ::Package:: *)

Begin["rule`"]


hold::usage = "rule`hold[x] \[LongDash] rules to replace {x -> Hold[x]}"

release::usage = "rule`release[x]  \[LongDash] rules to replace {(Hold|HoldForm)[x]->x}"

powerExpand::usage = "rule`powerExpand[x] \[LongDash] rule to pull x out of Power[] and Abs[]"

factor::usage = "rule`factor[x] \[LongDash] rule to factor x out of Plus[]"

pull::usage = "rule`pull[x] \[LongDash] rule to pull x out of Plus[]"

group::usage = "rule`group[x, func] \[LongDash] rule to replace {func[x] -> x}"

pullFactor::usage = "rule`pullFactor[pattern, func] \[LongDash] rule to pull (x:pattern) out of func[]assuming linearity of the func"

distribute::usage = "rule`distribute[outer_, inner_] rule to distribute outer w.r.t. inner"


Begin["`Private`"]


rule`hold[xs_List] := Join[
  {expr_Hold :> expr},
  Thread[xs -> Hold/@xs]
]
rule`hold[xs__] := rule`hold[{xs}]
rule`hold[pattern_] := {
  expr_Hold :> expr,
  (x:pattern) :> Hold[x]
}


rule`release[xs_List] := Flatten[{Hold[#]->#, HoldForm[#]->#}& /@ xs]
rule`release[xs__] := rule`release[{xs}]
rule`release[pattern_] := {
  (Hold[x_]/;MatchQ[x, pattern]) :> x,
  (HoldForm[x_]/;MatchQ[x, pattern]) :> x
}


rule`powerExpand[xs_List] := Join[
  {expr_Hold :> expr},
  Flatten[{
    (#^p_)^q_ :> #^Expand[(p q)],
    (expr_ * #^p_.)^q_ :> expr^q * #^(p q),
    Abs[expr_. * #^p_.] :> Abs[expr] * #^p
  }& /@ xs]
]
rule`powerExpand[xs__] := rule`powerExpand[{xs}]
rule`powerExpand[pattern_] := {
  expr_Hold :> expr,
  ((x:pattern)^p_)^q_ :> x^Expand[p q],
  (expr_ * (x:pattern)^p_.)^q_ :> expr^q * x^Expand[p q],
  Abs[expr_. * (x:pattern)^p_.] :> Abs[expr] * x^p
}


rule`factor[x_] := {
  expr_Hold :> expr,
  Plus[xs:Longest[(x * _.)..], other___] :> Plus[x * Total[{xs}/x], other]
}


rule`pull[x_] := {
  expr_Hold :> expr,
  (*Plus[x, others__] :> x (1 + Plus@@({others}/x)),*)
  Plus[expr:(x * _.), other__] :> x Total[{expr, other}/x]
}


rule`group[x_, func_:Expand] := func[x] -> x


rule`pullFactor[pattern_, func_] := func[a___, (x:pattern) * b_, c___] :> x * func[a, b, c]


rule`distribute[outer_, inner_] := expr:(outer[a___, inner[x_, xs__], b___]) :> Distribute[expr, inner]


End[]


End[]


Print[ToString@StringForm["[info]: '``' loaded", FileNameTake[$InputFileName, -3]]];
