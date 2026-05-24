Begin["rule`"];


rule`hypShift::usage = "rule to shift argument of hypergeometric function";


Begin["`Private`"];


rule`hypShift = {
  Hypergeometric2F1[a_, b_, c_, x_] :> (
    (Gamma[c] Gamma[c - a - b]) / (Gamma[c - a] Gamma[c - b]) * Hypergeometric2F1[a, b, a + b + 1 - c, 1 - x]
    + (Gamma[c] Gamma[a + b - c]) / (Gamma[a] Gamma[b]) (1 - x)^(c - a - b) * Hypergeometric2F1[c - a, c - b, 1 + c - a - b, 1 - x]
  )
};


End[];


End[];
