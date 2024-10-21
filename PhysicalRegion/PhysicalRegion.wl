BeginPackage["RG`PhysicalRegion`"];


getScalarProductRules::usage = "getScalarProductRules[process, constraints] evaluate scalar products for the process using constraints";

getInvariantRules::usage = "getInvariantRules[process, constraints] evaluate all scalar products for the process using constraints";

getPhysicalRegion::usage = "getPhysicalRegion[process, constraints] return inequalities for the physical region
getPhysicalRegion[process, rule`sp] uses rule`sp as scalar product rules"


Begin["`Private`"];


(* ::Section:: *)
(* Правила для инвариантов и скалярных произведений при введении обозначений *)


getPsProcConservation[procList:{{___}, {___}}] := {
	Flatten[procList],
	Flatten[{1, -1} * procList],
	Total[Flatten[{1, -1} * procList]]
};


getScalarProductRules[
	procList_,
	constraints_List
] := Module[
	{ps, proc, conservation, sps, var, vars, ruleVar, eqs},
	{ps, proc, conservation} = getPsProcConservation[procList];
	sps = Outer[Times, ps, ps] // Flatten // Union;
	vars = Array[var, Length[sps]];
	ruleVar = Thread[sps -> vars];
	eqs = constraints //
		Join[#, Thread[ps * conservation==0]]& //
		ExpandAll //
		ReplaceAll[ruleVar];
	Solve[eqs, vars] // ReplaceAll[Reverse/@ruleVar] // Flatten
];


getSP[ruleSps:{___Rule}] := Function[{expr},
  expr // Expand // ReplaceAll[ruleSps] // Simplify
];


getInvariantRules[procList_,ruleSps:{___Rule}] := Module[{
    ps, proc, conservation,
    eval = getSP[ruleSps],
    invariants
  },
	{ps, proc, conservation} = getPsProcConservation[procList];
	invariants = Outer[Simplify[Plus[#1, #2]^2]&, proc, proc] //
		Flatten //
		Union //
		Replace[#, (_?NumberQ) (expr_)^2 :> expr^2, 1]&;
	(#1 -> eval[#1])& /@ invariants
];
getInvariantRules[procList_, constraints:{__Equal}] := With[{
    ruleSps=getScalarProductRules[procList, constraints]
  },
  getInvariantRules[procList, ruleSps]
]


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

getPhysicalRegion[
	procList:{{a_, b_}, {c_, d_}},
	ruleSps:{___Rule}
] := Module[{
		ps, proc, conservation,
    ms, m2s,
    eval = getSP[ruleSps],
		s, t, u,
    region,
    eief, pipf
	},
	{ps, proc, conservation} = getPsProcConservation[procList];
	m2s = {a, b, c, d}^2 // Map[eval];
	ms = Sqrt/@m2s;
	s = eval[(a + b)^2];					(* (c + d)^2 *)
	t = eval[(a - c)^2];					(* (b - d)^2 *)
	u = eval[(a - d)^2];					(* (b - c)^2 *)
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
	) / 4 s;
	eief = (s + m2s[[1]] - m2s[[2]]) * (s + m2s[[3]] - m2s[[4]])/(4s);
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
getPhysicalRegion[procList_, constraints:{__Equal}] := With[
  {ruleSps = getScalarProductRules[procList, constraints]},
  getPhysicalRegion[procList, ruleSps]
];


(* ::Input:: *)
(* channelS = getPhysicalRegion[ *)
(* 	{{p1, p2}, {q1, q2}}, *)
(* 	{ *)
(* 		p1^2 == me^2, p2^2 == me^2, q1^2 == mmu^2, q2^2 == mmu^2, *)
(* 		(p1 + p2)^2==s, (p1 - q1)^2 == t *)
(* 	} *)
(* ] // Union *)

(* channelT = getPhysicalRegion[ *)
(* 	{{p1, q1}, {p2, q2}}, *)
(* 	{ *)
(* 		p1^2 == me^2, p2^2 == me^2, q1^2 == mmu^2, q2^2 == mmu^2, *)
(* 		(p1 + q1)^2==s, (p1 - p2)^2 == t *)
(* 	} *)
(* ] // Union *)


getPhysicalRegion[
	procList:{{a_}, {b_, c_, d_}},
	ruleSps:{___Rule}
] := Module[{
		ps, proc, conservation,
    ms, m2s,
    eval = getSP[ruleSps],
		s, t, u,
    eief, pipf,
    region
	},
	{ps, proc, conservation} = getPsProcConservation[procList];
	m2s = {a, b, c, d}^2 // Map[eval];
	ms = Sqrt/@m2s;
	s = eval[(a - b)^2];					(* (c+d)^2 *)
	t = eval[(a - c)^2];					(* (b+d)^2 *)
	u = eval[(a - d)^2];					(* (b+c)^2 *)
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
	region = region // Append[2eief -  2 pipf <= (m2s[[1]] + m2s[[3]]) - t  <= 2 eief + 2 pipf];
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
getPhysicalRegion[procList:{{}, {a_, b_, c_, d_}},ruleSps:{___Rule}] := {};
getPhysicalRegion[procList:{{a_, b_, c_, d_}, {}},ruleSps:{___Rule}] := {};


(* [TODO]: unimplemented, but may be necessary for example, for the  φ^4 - theory  *)
getPhysicalRegion[procList:{{a_, b_, c_}, {d_}},ruleSps:{___Rule}] := $Failed;


getPhysicalRegion[procList:{{a_}, {b_, c_}},ruleSps:{___Rule}] := Module[
  {},
  {ps, proc, conservation} = getPsProcConservation[procList];
	m2s = ps^2 // Map[eval];
	ms = Sqrt/@m2s;
	s = eval[a^2];
	region = {(ms[[2]] + ms[[3]])^2 <= s}
];


getPhysicalRegion[procList:{{b_, c_}, {a}},ruleSps:{___Rule}] := Module[
  {},
  {ps, proc, conservation} = getPsProcConservation[procList];
	m2s = ps^2 // Map[eval];
	ms = Sqrt/@m2s;
	s = eval[(b+c)^2];
	region = {m2s[[1]] == s}
];


End[];


EndPackage[];


Print[ToString@StringForm["[info]: '``' loaded", $InputFileName]];
