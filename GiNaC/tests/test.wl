Needs["RG`GiNaC`"];

Print[G[{0.1, 0.2}, 0.3]];
Print[N@G[{0.1, 0.2}, 0.3]];
Print[EvalG[{0.1, 0.2}, {Sqrt[0.99], 0.}, 0.3]];
Print[G[{1/2}, 1/3]];
Print[N@G[{1/2}, 1/3]];
Print[G[{Pi, Pi + I EulerGamma}, 1/3]];
Print[N@G[{Pi, Pi + I EulerGamma}, 1/3]];
Print[G[{0,1}, 1]];
Print[N@G[{-2, -1, 0}, 1/2]];
Print[EvalG[{0.1, 0.2}, {Sqrt[0.99]}, 0.3]];

Exit[0];
