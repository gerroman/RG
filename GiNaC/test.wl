Needs["RG`GiNaC`"];

Print[G[{0.1, 0.2}, 0.3]];

Print[EvalG[{0.1, 0.2}, {Sqrt[0.99], 0.}, 0.3]];
(* Uninstall[link]; *)
