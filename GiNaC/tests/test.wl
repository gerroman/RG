Needs["RG`Tools`"];
Needs["RG`GiNaC`"];

log[G[{0.1, 0.2}, 0.3]];
log[N@G[{0.1, 0.2}, 0.3]];
log[EvalG[{0.1, 0.2}, {Sqrt[0.99], 0.}, 0.3]];
log[G[{1/2}, 1/3]];
log[N@G[{1/2}, 1/3]];
log[G[{Pi, Pi + I EulerGamma}, 1/3]];
log[N@G[{Pi, Pi + I EulerGamma}, 1/3]];
log[G[{0,1}, 1]];
log[N@G[{-2, -1, 0}, 1/2]];
log[EvalG[{0.1, 0.2}, {Sqrt[0.99]}, 0.3]];

exit[0];
