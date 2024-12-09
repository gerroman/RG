BeginPackage["RG`Kinematics`PhysicalRegion`", {
  "RG`Lorentz`ScalarProduct`",
  "RG`Kinematics`ScalarProductRules`"
}]


GetPhysicalRegion::usage="GetPhysicalRegion[process, constraints] return inequalities in the physical region\nGetPhysicalRegion[process, spRules] uses rule`sp as scalar product rules"


Begin["`Private`"]


(* ::Section:: *)
(* Физическая область *)


(* Физическая область -- это область допустимых значений инвариантов *)


(* Появление физической области для процесса -- это следствие того, что *)
(* 1) реальные частицы должны быть на массовой поверхности, *)
(* 2) импульсы всех частиц в реакции подчиняются закону сохранения 4-импульса *)


(* Частично закон сохранения 4-импульсов уже учитывался *)
(* в скалярных произведениях и инвариантах*)
(* Но там учтены только соотношения типа равенств одного скалярного *)
(* произведения комбинации других *)


(* Физическая область подразумевает соотношения типа неравенств, *)
(* ограничивающих область допустимых значений инвариантов *)


(* Неравенства можно разделить на 2 типа: *)
(* 1) Простые следствия наличия реальных частиц *)
(* Например, наличие 2 реальных частиц с импульсами {p1, p2}, дает *)
(* простое следствия вида 2(p1, p2) >= 2 m1 m2 *)
(* [!] при этом не важно являются ли обе частицы начальными, или они обе конечные, или комбинация начальной конечной *)

(* Это дает простые следствия вида для инвариантов типа суммы и разницы импульсов *)
(* 1) sij = (pi + pj)^2 >= (mi + mj)^2,*)
(* 2) tij = (pi - pj)^2 <= (mi - mj)^2 *)

(* 2) Неравенства, следующие из кинематики процесса, *)
(* При вычислении подразумеваем процесс 2->2, *)
(* Выражения, содержащие корни, не раскрываем *)

GetSP[ruleSps:{___Rule}] := Function[{expr},
  expr // EvaluateScalarProducts[#, ruleSps]& // Simplify
];


GetPsProcConservation[procList:{{___},{___}}] := {
  Flatten[procList],
  Flatten[{1, -1} * procList],
  Total[Flatten[{1, -1} * procList]]
};


GetPhysicalRegion[
  procList:{{a_, b_}, {c_, d_}},
  ruleSps:{_Rule..}
] := Module[{
    ps, proc, conservation,
    ms, m2s,
    eval = GetSP[ruleSps],
    s, t, u,
    region,
    eief, pipf
  },
  {ps, proc, conservation} = GetPsProcConservation[procList];
  m2s = ScalarProduct/@{a, b, c, d} // Map[eval];
  ms = Sqrt/@m2s;
  s = eval[ScalarProduct[(a + b)]];					(* (c + d)^2 *)
  t = eval[ScalarProduct[(a - c)]];					(* (b - d)^2 *)
  u = eval[ScalarProduct[(a - d)]];					(* (b - c)^2 *)
  (* [comment]: check consistency of scalar products rules *)
  Assert[Simplify[s + t + u==Total[m2s]]];
  (* [comment]: 1) simple inequations *)
  region = {
    s >= (ms[[1]] + ms[[2]])^2,
    s >= (ms[[3]] + ms[[4]])^2,
    2 ms[[1]] ms[[3]] <= m2s[[1]] + m2s[[3]] - t,
    2 ms[[2]] ms[[4]] <= m2s[[2]] + m2s[[4]] - t
    (* [comment]: the following inequations can be obtained using permutations, *)
    (* [comment]: but they do not add information *)
    (*, 2 ms[[1]] ms[[4]] <= m2s[[1]] + m2s[[4]] - u*)
    (*, 2 ms[[2]] ms[[3]] <= m2s[[2]] + m2s[[3]] - u*)
  };
  (* [comment]: 2) kinematic inequations using c.m.s. *)
  pipf = (
    Sqrt[(s - (ms[[1]] - ms[[2]])^2)] *
    Sqrt[(s - (ms[[3]] - ms[[4]])^2)] *
    Sqrt[(s - (ms[[1]] + ms[[2]])^2)] *
    Sqrt[(s - (ms[[3]] + ms[[4]])^2)]
  ) / (4 s);
  eief = (s + m2s[[1]] - m2s[[2]]) * (s + m2s[[3]] - m2s[[4]])/(4 s);
  region = region // Append[2eief -  2 pipf <= (m2s[[1]] + m2s[[3]]) - t  <= 2 eief + 2 pipf];
  (* [comment]: the following equations can be obtained using permutations *)
  (* [comment]: but they do not add information *)
  (*eief = (s - m2s[[1]] + m2s[[2]]) (s - m2s[[3]] + m2s[[4]])/(4s);*)
  (*region = region // Append[2eief -  2 pipf <= (m2s[[2]] + m2s[[4]]) - t  <= 2 eief + 2 pipf];*)
  (*eief = (s + m2s[[1]] - m2s[[2]]) (s + m2s[[4]] - m2s[[3]])/(4s);*)
  (*region = region // Append[2eief -  2 pipf <= (m2s[[1]] + m2s[[4]]) - u  <= 2 eief + 2 pipf];*)
  (*eief = (s - m2s[[1]] + m2s[[2]]) (s - m2s[[4]] + m2s[[3]])/(4s);*)
  (*region = region // Append[2eief -  2 pipf <= (m2s[[2]] + m2s[[3]]) - u  <= 2 eief + 2 pipf];*)
  region
];
GetPhysicalRegion[procList:{{___}, {___}}, constraints:{_Equal..}] := With[
  {ruleSps = GetScalarProductRules[Flatten[{1,-1}*procList], constraints]},
  GetPhysicalRegion[procList, ruleSps]
];


GetPhysicalRegion[
  procList:{{a_}, {b_, c_, d_}},
  ruleSps:{___Rule}
] := Module[{
    ps, proc, conservation,
    ms, m2s,
    eval = GetSP[ruleSps],
    s, t, u,
    eief, pipf,
    region
  },
  {ps, proc, conservation} = GetPsProcConservation[procList];
  m2s = ScalarProduct/@{a, b, c, d} // Map[eval];
  ms = Sqrt/@m2s;
  s = eval[ScalarProduct[(a - b)]];					(* (c+d)^2 *)
  t = eval[ScalarProduct[(a - c)]];					(* (b+d)^2 *)
  u = eval[ScalarProduct[(a - d)]];					(* (b+c)^2 *)
  Assert[Simplify[s + t + u==Total[m2s]]];
  region = {
    (ms[[3]] + ms[[4]])^2 <= s,
    s <= (ms[[1]] - ms[[2]])^2,
    (ms[[2]] + ms[[4]])^2 <= t,
    t <= (ms[[1]] - ms[[3]])^2
    (* [comment]: the following inequations can be obtained using permutations, *)
    (* [comment]: but they do not add information *)
    (* , (ms[[2]] + ms[[3]])^2 <= u *)
    (* , u <= (ms[[1]] - ms[[4]])^2 *)
  };
  pipf = (
    Sqrt[((ms[[1]] - ms[[2]])^2 - s)]
    Sqrt[((ms[[1]] + ms[[2]])^2 - s)]
    Sqrt[(s - (ms[[3]] - ms[[4]])^2)]
    Sqrt[(s - (ms[[3]] + ms[[4]])^2)]
  ) / (4 s);
  eief = (
    Sqrt[(s + m2s[[1]] - m2s[[2]])^2]
    (s + m2s[[3]] - m2s[[4]])
  ) / (4 s);
  region = region // Append[2*eief -  2*pipf <= (m2s[[1]] + m2s[[3]]) - t  <= 2*eief + 2*pipf];
  (* [comment]: the following inequations can be obtained using permutations, *)
  (* [comment]: but they do not add information *)
  (* eief = (Sqrt[(s - m2s[[1]] + m2s[[2]])^2] (s - m2s[[3]] + m2s[[4]])) /(4 s); *)
  (* region = region // Append[2eief -  2 pipf <= t - (m2s[[2]] + m2s[[4]])  <= 2 eief + 2 pipf]; *)
  (* eief = (Sqrt[(s + m2s[[1]] - m2s[[2]])^2] (s - m2s[[3]] + m2s[[4]])) / (4 s); *)
  (* region = region // Append[2eief -  2 pipf <= (m2s[[1]] + m2s[[4]]) - u  <= 2 eief + 2 pipf]; *)
  (* eief = (Sqrt[(s - m2s[[1]] + m2s[[2]])^2] (s - m2s[[4]] + m2s[[3]])) /(4 s); *)
  (* region = region // Append[2eief -  2 pipf <= u - (m2s[[2]] + m2s[[3]])  <= 2 eief + 2 pipf]; *)
  region
];


(* [comment]: seems impossible due to energy conservation law *)
GetPhysicalRegion[procList:{{}, {a_, b_, c_, d_}},ruleSps:{___Rule}] := {};
GetPhysicalRegion[procList:{{a_, b_, c_, d_}, {}},ruleSps:{___Rule}] := {};


(* [TODO]: unimplemented, but may be necessary for example, for the  \[CurlyPhi]^4 - theory  *)
GetPhysicalRegion[procList:{{a_, b_, c_}, {d_}},ruleSps:{___Rule}] := $Failed;


GetPhysicalRegion[procList:{{a_}, {b_, c_}},ruleSps:{___Rule}] := Module[
  {eval = GetSP[ruleSps], ps, proc, conservation, m2s, ms, s, region},
  {ps, proc, conservation} = GetPsProcConservation[procList];
  m2s = (ScalarProduct/@ps) // Map[eval];
  ms = Sqrt/@m2s;
  s = eval[ScalarProduct[a]];
  region = {(ms[[2]] + ms[[3]])^2 <= s}
];


GetPhysicalRegion[procList:{{b_, c_}, {a}},ruleSps:{___Rule}] := Module[
  {eval = GetSP[ruleSps], ps, proc, conservation, m2s, ms, s, region},
  {ps, proc, conservation} = GetPsProcConservation[procList];
  m2s = ScalarProduct/@ps // Map[eval];
  ms = Sqrt/@m2s;
  s = eval[ScalarProduct[(b + c)]];
  region = {m2s[[1]] == s}
];


End[]


EndPackage[]


fileStamp[]
