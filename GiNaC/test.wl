Needs["RG`GiNaC`"];

SetAttributes[show, HoldFirst]
show[expr_] := Write["stderr", ToString[OutputForm[HoldForm[expr]] == expr]];
show[G[{0.1, 0.2}, 0.3]];
show[N@G[{0.1, 0.2}, 0.3]];
show[EvalG[{0.1, 0.2}, {Sqrt[0.99], 0.}, 0.3]];
show[G[{1/2}, 1/3]];
show[N@G[{1/2}, 1/3]];
show[G[{Pi, Pi + I EulerGamma}, 1/3]];
show[N@G[{Pi, Pi + I EulerGamma}, 1/3]];
