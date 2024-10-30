(* ::Package:: *)

BeginPackage["RG`PhysicalRegion`"];


getScalarProductRules::usage = "getScalarProductRules[process, constraints] evaluate scalar products for the process using constraints";

getInvariantRules::usage = "getInvariantRules[process, constraints] evaluate all scalar products for the process using constraints";

getPhysicalRegion::usage = "getPhysicalRegion[process, constraints] return inequalities for the physical region
getPhysicalRegion[process, rule`sp] uses rule`sp as scalar product rules"


Begin["`Private`"];


(* ::Section:: *)
(* \:041f\:0440\:0430\:0432\:0438\:043b\:0430 \:0434\:043b\:044f \:0438\:043d\:0432\:0430\:0440\:0438\:0430\:043d\:0442\:043e\:0432 \:0438 \:0441\:043a\:0430\:043b\:044f\:0440\:043d\:044b\:0445 \:043f\:0440\:043e\:0438\:0437\:0432\:0435\:0434\:0435\:043d\:0438\:0439 \:043f\:0440\:0438 \:0432\:0432\:0435\:0434\:0435\:043d\:0438\:0438 \:043e\:0431\:043e\:0437\:043d\:0430\:0447\:0435\:043d\:0438\:0439 *)


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
(* \:0424\:0438\:0437\:0438\:0447\:0435\:0441\:043a\:0430\:044f \:043e\:0431\:043b\:0430\:0441\:0442\:044c *)


(* \:0424\:0438\:0437\:0438\:0447\:0435\:0441\:043a\:0430\:044f \:043e\:0431\:043b\:0430\:0441\:0442\:044c -- \:044d\:0442\:043e \:043e\:0431\:043b\:0430\:0441\:0442\:044c \:0434\:043e\:043f\:0443\:0441\:0442\:0438\:043c\:044b\:0445 \:0437\:043d\:0430\:0447\:0435\:043d\:0438\:0439 \:0438\:043d\:0432\:0430\:0440\:0438\:0430\:043d\:0442\:043e\:0432 *)


(* \:041f\:043e\:044f\:0432\:043b\:0435\:043d\:0438\:0435 \:0444\:0438\:0437\:0438\:0447\:0435\:0441\:043a\:043e\:0439 \:043e\:0431\:043b\:0430\:0441\:0442\:0438 \:0434\:043b\:044f \:043f\:0440\:043e\:0446\:0435\:0441\:0441\:0430 -- \:044d\:0442\:043e \:0441\:043b\:0435\:0434\:0441\:0442\:0432\:0438\:0435 \:0442\:043e\:0433\:043e, \:0447\:0442\:043e *)
(* 1) \:0440\:0435\:0430\:043b\:044c\:043d\:044b\:0435 \:0447\:0430\:0441\:0442\:0438\:0446\:044b \:0434\:043e\:043b\:0436\:043d\:044b \:0431\:044b\:0442\:044c \:043d\:0430 \:043c\:0430\:0441\:0441\:043e\:0432\:043e\:0439 \:043f\:043e\:0432\:0435\:0440\:0445\:043d\:043e\:0441\:0442\:0438, *)
(* 2) \:0438\:043c\:043f\:0443\:043b\:044c\:0441\:044b \:0432\:0441\:0435\:0445 \:0447\:0430\:0441\:0442\:0438\:0446 \:0432 \:0440\:0435\:0430\:043a\:0446\:0438\:0438 \:043f\:043e\:0434\:0447\:0438\:043d\:044f\:044e\:0442\:0441\:044f \:0437\:0430\:043a\:043e\:043d\:0443 \:0441\:043e\:0445\:0440\:0430\:043d\:0435\:043d\:0438\:044f 4-\:0438\:043c\:043f\:0443\:043b\:044c\:0441\:0430 *)


(* \:0427\:0430\:0441\:0442\:0438\:0447\:043d\:043e \:0437\:0430\:043a\:043e\:043d \:0441\:043e\:0445\:0440\:0430\:043d\:0435\:043d\:0438\:044f 4-\:0438\:043c\:043f\:0443\:043b\:044c\:0441\:043e\:0432 \:0443\:0436\:0435 \:0443\:0447\:0438\:0442\:044b\:0432\:0430\:043b\:0441\:044f *)
(* \:0432 \:0441\:043a\:0430\:043b\:044f\:0440\:043d\:044b\:0445 \:043f\:0440\:043e\:0438\:0437\:0432\:0435\:0434\:0435\:043d\:0438\:044f\:0445 \:0438 \:0438\:043d\:0432\:0430\:0440\:0438\:0430\:043d\:0442\:0430\:0445*)
(* \:041d\:043e \:0442\:0430\:043c \:0443\:0447\:0442\:0435\:043d\:044b \:0442\:043e\:043b\:044c\:043a\:043e \:0441\:043e\:043e\:0442\:043d\:043e\:0448\:0435\:043d\:0438\:044f \:0442\:0438\:043f\:0430 \:0440\:0430\:0432\:0435\:043d\:0441\:0442\:0432 \:043e\:0434\:043d\:043e\:0433\:043e \:0441\:043a\:0430\:043b\:044f\:0440\:043d\:043e\:0433\:043e *)
(* \:043f\:0440\:043e\:0438\:0437\:0432\:0435\:0434\:0435\:043d\:0438\:044f \:043a\:043e\:043c\:0431\:0438\:043d\:0430\:0446\:0438\:0438 \:0434\:0440\:0443\:0433\:0438\:0445 *)


(* \:0424\:0438\:0437\:0438\:0447\:0435\:0441\:043a\:0430\:044f \:043e\:0431\:043b\:0430\:0441\:0442\:044c \:043f\:043e\:0434\:0440\:0430\:0437\:0443\:043c\:0435\:0432\:0430\:0435\:0442 \:0441\:043e\:043e\:0442\:043d\:043e\:0448\:0435\:043d\:0438\:044f \:0442\:0438\:043f\:0430 \:043d\:0435\:0440\:0430\:0432\:0435\:043d\:0441\:0442\:0432, *)
(* \:043e\:0433\:0440\:0430\:043d\:0438\:0447\:0438\:0432\:0430\:044e\:0449\:0438\:0445 \:043e\:0431\:043b\:0430\:0441\:0442\:044c \:0434\:043e\:043f\:0443\:0441\:0442\:0438\:043c\:044b\:0445 \:0437\:043d\:0430\:0447\:0435\:043d\:0438\:0439 \:0438\:043d\:0432\:0430\:0440\:0438\:0430\:043d\:0442\:043e\:0432 *)


(* \:041d\:0435\:0440\:0430\:0432\:0435\:043d\:0441\:0442\:0432\:0430 \:043c\:043e\:0436\:043d\:043e \:0440\:0430\:0437\:0434\:0435\:043b\:0438\:0442\:044c \:043d\:0430 2 \:0442\:0438\:043f\:0430: *)
(* 1) \:041f\:0440\:043e\:0441\:0442\:044b\:0435 \:0441\:043b\:0435\:0434\:0441\:0442\:0432\:0438\:044f \:043d\:0430\:043b\:0438\:0447\:0438\:044f \:0440\:0435\:0430\:043b\:044c\:043d\:044b\:0445 \:0447\:0430\:0441\:0442\:0438\:0446 *)
(* \:041d\:0430\:043f\:0440\:0438\:043c\:0435\:0440, \:043d\:0430\:043b\:0438\:0447\:0438\:0435 2 \:0440\:0435\:0430\:043b\:044c\:043d\:044b\:0445 \:0447\:0430\:0441\:0442\:0438\:0446 \:0441 \:0438\:043c\:043f\:0443\:043b\:044c\:0441\:0430\:043c\:0438 {p1, p2}, \:0434\:0430\:0435\:0442 *)
(* \:043f\:0440\:043e\:0441\:0442\:043e\:0435 \:0441\:043b\:0435\:0434\:0441\:0442\:0432\:0438\:044f \:0432\:0438\:0434\:0430 2(p1, p2) >= 2 m1 m2 *)
(* [!] \:043f\:0440\:0438 \:044d\:0442\:043e\:043c \:043d\:0435 \:0432\:0430\:0436\:043d\:043e \:044f\:0432\:043b\:044f\:044e\:0442\:0441\:044f \:043b\:0438 \:043e\:0431\:0435 \:0447\:0430\:0441\:0442\:0438\:0446\:044b \:043d\:0430\:0447\:0430\:043b\:044c\:043d\:044b\:043c\:0438, \:0438\:043b\:0438 \:043e\:043d\:0438 \:043e\:0431\:0435 \:043a\:043e\:043d\:0435\:0447\:043d\:044b\:0435, \:0438\:043b\:0438 \:043a\:043e\:043c\:0431\:0438\:043d\:0430\:0446\:0438\:044f \:043d\:0430\:0447\:0430\:043b\:044c\:043d\:043e\:0439 \:043a\:043e\:043d\:0435\:0447\:043d\:043e\:0439 *)

(* \:042d\:0442\:043e \:0434\:0430\:0435\:0442 \:043f\:0440\:043e\:0441\:0442\:044b\:0435 \:0441\:043b\:0435\:0434\:0441\:0442\:0432\:0438\:044f \:0432\:0438\:0434\:0430 \:0434\:043b\:044f \:0438\:043d\:0432\:0430\:0440\:0438\:0430\:043d\:0442\:043e\:0432 \:0442\:0438\:043f\:0430 \:0441\:0443\:043c\:043c\:044b \:0438 \:0440\:0430\:0437\:043d\:0438\:0446\:044b \:0438\:043c\:043f\:0443\:043b\:044c\:0441\:043e\:0432 *)
(* 1) sij = (pi + pj)^2 >= (mi + mj)^2,*)
(* 2) tij = (pi - pj)^2 <= (mi - mj)^2 *)

(* 2) \:041d\:0435\:0440\:0430\:0432\:0435\:043d\:0441\:0442\:0432\:0430, \:0441\:043b\:0435\:0434\:0443\:044e\:0449\:0438\:0435 \:0438\:0437 \:043a\:0438\:043d\:0435\:043c\:0430\:0442\:0438\:043a\:0438 \:043f\:0440\:043e\:0446\:0435\:0441\:0441\:0430, *)

(* \:041f\:0440\:0438 \:0432\:044b\:0447\:0438\:0441\:043b\:0435\:043d\:0438\:0438 \:043f\:043e\:0434\:0440\:0430\:0437\:0443\:043c\:0435\:0432\:0430\:0435\:043c \:043f\:0440\:043e\:0446\:0435\:0441\:0441 2->2, *)
(* \:0412\:044b\:0440\:0430\:0436\:0435\:043d\:0438\:044f, \:0441\:043e\:0434\:0435\:0440\:0436\:0430\:0449\:0438\:0435 \:043a\:043e\:0440\:043d\:0438, \:043d\:0435 \:0440\:0430\:0441\:043a\:0440\:044b\:0432\:0430\:0435\:043c *)

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


(* [TODO]: unimplemented, but may be necessary for example, for the  \[CurlyPhi]^4 - theory  *)
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
