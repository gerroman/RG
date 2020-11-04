BeginTestSection["TestKinematics"]

Needs["RG`Kinematics`"];

VerificationTest[(* 1 *)
	setInvariants[List[l, p, Times[-1, prime`l], Times[-1, prime`p]], List[m, M, m, M], List[Rule[q, Plus[l, Times[-1, prime`l]]], Rule[K, Plus[l, prime`l]], Rule[P, Plus[p, prime`p]]], List[sp[l, p], sp[q, q]]]
	,
	List[Rule[sp[K, K], Plus[Times[4, Power[m, 2]], Times[-1, sp[q, q]]]], Rule[sp[K, l], Plus[Times[2, Power[m, 2]], Times[-1, Times[Times[1, Power[2, -1]], sp[q, q]]]]], Rule[sp[K, prime`l], Plus[Times[2, Power[m, 2]], Times[-1, Times[Times[1, Power[2, -1]], sp[q, q]]]]], Rule[sp[K, p], Plus[Times[2, sp[l, p]], Times[Times[1, Power[2, -1]], sp[q, q]]]], Rule[sp[K, prime`p], Plus[Times[2, sp[l, p]], Times[Times[1, Power[2, -1]], sp[q, q]]]], Rule[sp[K, P], Plus[Times[4, sp[l, p]], sp[q, q]]], Rule[sp[K, q], 0], Rule[sp[l, l], Power[m, 2]], Rule[sp[l, prime`l], Plus[Power[m, 2], Times[-1, Times[Times[1, Power[2, -1]], sp[q, q]]]]], Rule[sp[l, prime`p], Plus[sp[l, p], Times[Times[1, Power[2, -1]], sp[q, q]]]], Rule[sp[l, P], Plus[Times[2, sp[l, p]], Times[Times[1, Power[2, -1]], sp[q, q]]]], Rule[sp[l, q], Times[Times[1, Power[2, -1]], sp[q, q]]], Rule[sp[prime`l, prime`l], Power[m, 2]], Rule[sp[prime`l, p], Plus[sp[l, p], Times[Times[1, Power[2, -1]], sp[q, q]]]], Rule[sp[prime`l, prime`p], sp[l, p]], Rule[sp[prime`l, P], Plus[Times[2, sp[l, p]], Times[Times[1, Power[2, -1]], sp[q, q]]]], Rule[sp[prime`l, q], Times[Times[-1, Times[1, Power[2, -1]]], sp[q, q]]], Rule[sp[p, p], Power[M, 2]], Rule[sp[p, prime`p], Plus[Power[M, 2], Times[-1, Times[Times[1, Power[2, -1]], sp[q, q]]]]], Rule[sp[p, P], Plus[Times[2, Power[M, 2]], Times[-1, Times[Times[1, Power[2, -1]], sp[q, q]]]]], Rule[sp[p, q], Times[Times[-1, Times[1, Power[2, -1]]], sp[q, q]]], Rule[sp[prime`p, prime`p], Power[M, 2]], Rule[sp[prime`p, P], Plus[Times[2, Power[M, 2]], Times[-1, Times[Times[1, Power[2, -1]], sp[q, q]]]]], Rule[sp[prime`p, q], Times[Times[1, Power[2, -1]], sp[q, q]]], Rule[sp[P, P], Plus[Times[4, Power[M, 2]], Times[-1, sp[q, q]]]], Rule[sp[P, q], 0]]	
]

EndTestSection[]
