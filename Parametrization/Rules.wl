Needs["RG`Integrate`"]


Global`mi0::usage="mi0 \[LongDash] imaginary part of the propagators denominators (- \[ImaginaryI] \[CurlyEpsilon])"

rule`mi0::usage = "rule`mi0  \[LongDash] replaces mi0 * (_Hold) -> mi0"

rule`imaginary::usage "rule`imaginary  \[LongDash] substitute (expr - \[ImaginaryI] \[CurlyEpsilon])^p assuming expr is negative"


rule`f21::usage="rule`f21 \[LongDash] substitute integral representation of Hypergeometric2F1"
rule`f21arg::usage = "rule`f21arg \[LongDash] argument shift {z -> 1 - z} of Hypergeometric2F1"
rule`f21ind::usage = "rule`f21ind \[LongDash] index shift of {} Hypergeometric2F1"
rule`appell::usage="rule`f21 \[LongDash] substitute integral representation of AppellF1"

rule`f21matrix::usage = "rule`f21matrix \[LongDash] return differential system matrix for
{Hypergeometric2F1[a, b, c, z], D[Hypergeometric2F1[a, b, c, z], z]}"

rule`f21inv::usage = "rule`f21inv \[LongDash] inverse rule`f21 substitution"


Begin["rule`Private`"]


(* ::Section:: *)
(* Правила работы с мнимыми добавками *)
Format[Global`mi0, TraditionalForm] := DisplayForm[RowBox[{"(","-","\[ImaginaryI]","0",")"}]];
Unprotect[Plus];
Format[Plus[Global`mi0, a_], TraditionalForm] := DisplayForm[RowBox[{ToBoxes[a,TraditionalForm], "-","\[ImaginaryI]0"}]];
Format[Plus[(-1)*Global`mi0, a_], TraditionalForm] := DisplayForm[RowBox[{ToBoxes[a,TraditionalForm], "+","\[ImaginaryI]0"}]];
Protect[Plus];


rule`mi0[x_] := Global`mi0 * x^(p_.) -> Global`mi0;
rule`mi0[xs_List] := (Global`mi0 * #^(p_.) -> Global`mi0)& /@ xs;
rule`imaginary = {
  (expr_ + Global`mi0)^(p_.) :> (Print[StringForm["[assumptions]: (``) < 0", expr]];(-expr)^p Exp[-I Pi p]),
  (expr_ - Global`mi0)^(p_.) :> (Print[StringForm["[assumptions]: (``) < 0", expr]];(-expr)^p Exp[+I Pi p])
};


(* ::Section:: *)
(* Правила работы с гипергеометрической функцией *)

rule`f21 = {
  integrate[(1 - tau_)^(q_.)*(tau_)^(p_.)*(1 + (tau_)*(z_.))^r_., {tau_, 0, 1}] :>
    (Gamma[1 + p]*Gamma[1 + q] / Gamma[2 + p + q]) *
       Hypergeometric2F1[-r, 1 + p , 2 + p + q, -z],
  integrate[(1 - tau_)^(q_.)*(1 + (tau_) * (z_.))^(r_.), {tau_, 0, 1}] :>
    (q + 1)^(-1) * Hypergeometric2F1[-r, 1, 2 + q, -z],
  integrate[(tau_)^(p_.)*(1 + (tau_)*(z_.))^(r_.), {tau_, 0, 1}] :>
    (p + 1)^(-1) * Hypergeometric2F1[-r, 1 + p, 2 + p, -z],
  integrate[(tau_)^(p_.) (1 + (tau_)*(z_.))^(r_.) (1 + tau_)^(s_.), {tau_, 0, Infinity}] :> With[
    {q = -(s + 2 + p + r)},
    (Gamma[1 + p]*Gamma[1 + q] / Gamma[2 + p + q]) *
       Hypergeometric2F1[-r, 1 + p , 2 + p + q, 1-z]
  ],
  integrate[(1 +(tau_) (z_.))^(r_.) (1 + (tau_))^(s_.), {tau_, 0, Infinity}] :> With[
    {q = -(s + 2 + r)},
    (q + 1)^(-1) * Hypergeometric2F1[-r, 1, 2 + q, 1-z]
  ],
  integrate[(tau_)^(p_.) (1 + (tau_) (z_.))^(r_.), {tau_, 0, Infinity}] :> With[
    {q = -(2 + p + r)},
    (Gamma[1 + p]*Gamma[1 + q] / Gamma[2 + p + q]) *
    Hypergeometric2F1[-r, 1 + p , 2 + p + q, 1 - z]
  ]
}


rule`f21inv = (
  Hypergeometric2F1[mr_, p1_, pq2_, mz_] :> With[
    {
      tau=With[{x=ToExpression["x"]}, If[ValueQ[x], Unique["x$"], x]],
      p = p1 - 1,
      q = pq2 - p1 - 1,
      z = -mz,
      r = -mr
    },
    (Gamma[1 + p]*Gamma[1 + q] / Gamma[2 + p + q])^(-1) *
      integrate[(1 - tau)^q*(tau)^p*(1 + tau*z)^r, {tau, 0, 1}]
  ]
);


rule`f21ind = {
  Hypergeometric2F1[a_, b_, c_, z_] :>
    Together[(1 - z)]^(-a) Hypergeometric2F1[a, c - b, c, Together[z/(z - 1)]]
}

rule`f21arg = (
  (f_.) Hypergeometric2F1[a_, b_, c_, z_] :>
    f ((Gamma[c]*Gamma[c - a - b])/(Gamma[c - a]*Gamma[c - b])) *
      Hypergeometric2F1[a, b, a + b + 1 - c, 1 - z] +
    f ((Gamma[c]*Gamma[a + b - c])/(Gamma[a]*Gamma[b])) *
      (1 - z)^(c - a - b) * Hypergeometric2F1[c - a, c - b, 1 + c - a - b, 1 - z]
)

rule`appell = {
  integrate[t_^p_. (1 - t_)^q_. (1 + t_ x_)^r_. (1 + t_ y_)^s_., {t_, 0, 1}] :>
    (Gamma[p + 1] Gamma[q + 1])/ Gamma[p + q + 2] *
      AppellF1[p + 1, -r, -s, p + q + 2, -x, -y],
  integrate[(1 - t_)^q_. (1 + t_ x_)^r_. (1 + t_ y_)^s_., {t_, 0, 1}] :>
    (Gamma[1] Gamma[q + 1])/ Gamma[q + 2] *
      AppellF1[1, -r, -s, q + 2, -x, -y],
  integrate[t_^p_. (1 + t_ x_)^r_. (1 + t_ y_)^s_., {t_, 0, 1}] :>
    (Gamma[p + 1] Gamma[1]) / Gamma[p + 2] *
      AppellF1[p + 1, -r, -s, p + 2, -x, -y],
  integrate[(1 + t_ x_)^r_. (1 + t_ y_)^s_., {t_, 0, 1}] :>
    (Gamma[1] Gamma[1])/Gamma[2] *
      AppellF1[1, -r, -s, 2, -x, -y]
}


rule`f21matrix = {
  Hypergeometric2F1[a_, b_, c_, z_] :> {
    {0, 1},
    {-((a*b)/(-1 + z)) + (a*b)/z, (-1 - a - b + c)/(-1 + z) - c/z}
  }
}


End[]


RG`Scripts`fileStamp[]
