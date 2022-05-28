(* ::Package:: *)

BeginPackage["RG`Groups`", {"RG`Calculation`"}]


group`reset::usage = "
  group`reset[] clear all definitions
"


group`elems::usage = "
  group`elems \[LongDash] list of elems in vertices symmetry group
"
group`id::usage = "
  group`id \[LongDash] return identity of group
"


group`nvertices::usage = "
  nvertices \[LongDash] number of points for permutation group
"
group`permutationrule::usage = "
  group`permutationrule \[LongDash] rule to convert elems to Cycles
"
group`permutation::usage = "
  group`permutation[expr] \[LongDash] return permutation for group element expr
"
group`vertexrepresentation::usage = "
  group`vertexrepresentation[expr] return regular representation of expr
  group`regularrepresentation[expr]
"


group`evaluate::usage = "
  group`evaluate[expr] evaluates products usign group`rule
"
group`producttable::usage = "
  group`producttable[] calculate table of products of all elements
"


group`draw::usage = "
  group`draw[expr, func, labels] draw vertices before and after performing symmetry transformation
"


group`order::usage = "
  group`order[expr] return the order of expr in the group
  group`order[] return the order (elements number) of the group
"
group`inverse::usage = "
  group`inverse[expr] return inverse element in the group
"


group`class::usage = "
  group`class[expr] return class of conjugated elements
"
group`classes::usage = "
  group`classes \[LongDash] return group classes of cojugate elements
"
group`nclasses::usage = "
  group`nclasses return number of classes of conjugated elements
"


group`groupQ::usage = "
  group`groupQ[elems] cheks if elems make a (sub)group
"
group`subgroups::usage = "
  group`subgroups[] return all subgroups in the group
"
group`rightclasses::usage = "
  group`rightclasses[subgroup] return right classes
"
group`leftclasses::usage = "
  group`leftclasses[subgroup] return left classes
"
group`invariantGroupQ::usage = "
  group`invarinatGroupQ[elems] cheks if elems make a invariant (sub)group
"
group`invariantsubgroups::usage = "
  group`invariantsubgroups[] return all subgroups in the group
"


group`charactertable::usage = "
  group`charactertable irreducible representation character table
"
group`character::usage = "
  group`character[expr, repr] return character of expr in representation
  group`character[classes, repr] return character for the first elements in the classes
"
\[Chi]::usage = "
  \[Chi] represent character
"


group`regularrepresentation::usage = "
  group`regularrepresentation[expr] return regular representation of expr
  group`regularrepresentation[]
"


group`irreducibledimensions::usage = "
  group`irreducibledimensions try to calculate irreducible dimensions for the group g
"
group`irreduciblesplit::usage = "
  group`irreduciblesplit[repr] split representation to irreducible using group`charactertable
"

group`formatpermutation::usage = "
  group`formatpermutation[n] format Cycles as matrix 2 x n
"


Begin["`Private`"]


Unprotect[PermutationProduct, PermutationPower];
Format[PermutationPower[x_, n_], TraditionalForm] := x^n;
Format[PermutationProduct[x_, y__], TraditionalForm] := Row[Reverse@{x, y}];
Protect[PermutationProduct, PermutationPower];


group`reset[] := (
  Echo["[Info]: reset group definitions ..."];
  Clear[
	  group`elems,
    group`nvertices,
	  group`permutationrule,
		group`inversepermutationrule,
		group`representrule,
		group`inverserepresentrule,
	  group`charactertable,
	  group`classes
  ];
  group`classes := group`classes = Union[group`class /@ group`elems] // group`sortsets;

  group`inversepermutationrule := group`inversepermutationrule = Reverse /@ (
	  Rule @@@ (group`elems // Map[process[ReplaceRepeated[#, group`permutationrule]&]])
	);

	group`permutation[] := group`permutation[] = group`elems // Map[process[group`permutation] /* Apply[Equal]];

  group`inverserepresentrule := group`inverserepresentrule = Reverse /@ (Rule @@@ (group`elems // Map[process[group`matrixevaluate, ReplaceRepeated[#, group`representrule]&]]));

  group`subgroups[] := group`subgroups[] = Flatten[group`subgroups /@ (Divisors[group`order[]][[2;;-2]]), 1];

  group`invariantsubgroups[] := group`invariantsubgroups[] = group`subgroups[] // Select[group`invariantGroupQ];

	group`id := group`id = First[group`elems];

	group`producttable[] := group`producttable[] = Outer[
    group`evaluate[PermutationProduct[#2, #1]]&,
 	  group`elems,
	  group`elems
	];

	group`regularrepresentation[] := group`regularrepresentation[] = group`elems // Map[rewriteIt[group`regularrepresentation]];

  group`vertexrepresentation[] := group`vertexrepresentation[] = group`elems // Map[rewriteIt[group`vertexrepresentation]];

  group`irreducibledimensions := group`irreducibledimensions = Module[{dim, dims, i, n = group`order[], nc = group`nclasses, solution},
	  dims = Array[dim, nc];
    solution = Solve[
	    And[
		    n == Total[dims^2],
			  And @@ Thread[dims > 0],
			  LessEqual @@ dims
		  ],
		  dims,
		  Integers
	  ];
	  If[Length[solution] != 1, Print["[Error] irreducible dimensions yields multiple solution"]];
	  dims /. First[solution]
  ];

  group`classidx := With[
	  {rule = (group`classes -> Range[Length[group`classes]] // Thread // Map[Thread] // Flatten // Sort // Reverse)},
    group`classidx = Function[{expr}, expr /. rule]
  ];
);


group`reset[];


group`matrixevaluate = ReplaceRepeated[#, {
	  PermutationPower -> MatrixPower,
		PermutationProduct[a_, b__] :> Dot@@Reverse[{a, b}]
	}
]&


group`evaluate[expr_] := Which[
  ValueQ[group`permutationrule],
	  expr // ReplaceRepeated[#, group`permutationrule]& // ReplaceRepeated[#, group`inversepermutationrule]&,
	ValueQ[group`representrule],
	  expr // group`matrixevaluate // ReplaceRepeated[#, group`representrule]& // ReplaceAll[#, group`inverserepresentrule]&,
	True, (Echo["[Warning] there is no group definitions..."]; expr)
]

group`producttable[expr_List/;VectorQ[expr]] := Outer[
  group`evaluate[PermutationProduct[#2, #1]]&,
	expr,
	expr
]
group`producttable[expr_List/;MatrixQ[expr]] := Outer[
  group`evaluate[PermutationProduct[#2, #1]]&,
	expr,
	expr
] // Map[Transpose] // Map[Map[Flatten/*Union/*group`sort]]


group`formatpermutation[n_] := With[
  {r = Range[n]},
	p \[Function] {r, Permute[r, p]}
]

group`permutation[expr_] := With[
  {func={#, Permute[#, (expr/.group`permutationrule)]}&},
  func[Range[group`nvertices]]
]


group`draw[expr_, func_] := With[
  {p = group`permutation[expr]},
  func[First[p]] -> func[Last[p]]
]
group`draw[expr_, func_, labels_] := With[
  {p = group`permutation[expr]},
  func[labels[[First[p]]]] -> func[labels[[Last[p]]]]
]


group`order[expr_] := Length[
  NestWhileList[
	  group`evaluate[PermutationProduct[#, expr]]&,
		expr,
	  (# =!= group`id)&
	]
]
group`order[] := Length[group`elems]


group`inverse[expr_] := group`evaluate[PermutationPower[expr, -1]]


group`sortfunction := FirstPosition[group`elems, #]&;
group`sort := SortBy[group`sortfunction];
group`sortsets := Map[group`sort] /* SortBy[First /* group`sortfunction];


group`class[expr_] := Union[group`evaluate[PermutationProduct[#, expr, group`inverse[#]]]& /@ group`elems] // group`sort;
group`nclasses := Length[group`classes];


group`groupQ[expr_List] := With[{elems = Union[group`evaluate[expr]]}, Which[
  Not@MemberQ[expr, group`id], (
	  (* Echo["[Error]: groupQ: there is no in identity element"];*)
		False
	),
	Length[elems] != Length[expr], (
	  (* Echo["[Error]: groupQ: there are duplicates"]; *)
		False
	),
	elems != Union[group`inverse /@ expr], (
	  (* Echo["[Error]: groupQ: inverse element does not exist"]; *)
		False
	),
	With[{products = Union @ Flatten @ group`producttable[expr]},
    products =!= elems
	], (
	  (* Echo["[Error]: groupQ: not all products does exist"]; *)
		False
	),
	True, True
]];
group`groupQ[] := group`groupQ[group`elems];


group`subgroups[n_Integer] := Subsets[group`elems, {n}] // Select[group`groupQ] // Map[group`sort]


group`rightclasses[expr_List] := Union[
  Union /@ Outer[
	  group`evaluate[PermutationProduct[#2, #1]]&,
		group`elems,
		expr
	]
] // group`sortsets;

group`leftclasses[expr_List] := Union[
  Union /@ Outer[
	  group`evaluate[PermutationProduct[#1, #2]]&,
		group`elems,
		expr
	]
] // group`sortsets;


group`invariantGroupQ[expr_List] := group`leftclasses[expr] === group`rightclasses[expr]
group`invariantsubgroups[n_Integer] := group`subgroups[n] // Select[group`invariantGroupQ]


group`regularrepresentation[expr_] := With[
  {n = group`order[]},
	Array[
	  Function[{i, j},
	    Boole[
			  group`evaluate[PermutationProduct[group`elems[[j]], expr]] ===
			  group`elems[[i]]
			]
		],
	  {n, n}
	]
]


group`vertexrepresentation[expr_] := With[
  {
	  n = group`nvertices,
		p = Permute[
		  Range[group`nvertices],
		  expr/.group`permutationrule
		]
	},
  Array[Boole[p[[#1]] == #2]&, {n, n}]
]


group`character[expr_, repr_List] := Tr[expr /. Rule@@@(Reverse@Sort@repr)]
group`character[repr_List] := group`classes // Map[First] // Map[rewriteIt[{\[Chi]}, {group`character[#, repr]&}]]


group`irreduciblesplit[repr_List] := With[
  {
	  chs = Conjugate[Last /@ group`character[repr]],
		ps = Length /@ group`classes,
		n = group`order[]
	},
	(Total[ps * chs * #] / n &) /@ group`charactertable
];
group`irreduciblesplit[chs:{_Integer..}] := With[
  {
	  ps = Length /@ group`classes,
		n = group`order[]
	},
	(Total[ps * chs * #] / n &) /@ group`charactertable
];



group`irreduciblecharacter[i_Integer][expr_] := With[
  {idx = group`classidx[expr]},
  group`charactertable[[i, idx]]
];


group`projectors[repr_List] := Module[
  {
	  classes = group`classes,
		dims = group`charactertable[[All, 1]],
		n = group`order[]
	},
	Table[
	  (dims[[i]]/n) * Total[group`irreduciblecharacter[i][#] * (# /. Rule@@@(Reverse@Sort@repr))& /@ group`elems],
		{i, Length[dims]}
	]
]


group`checkcharactertable[] := Module[
  {
	  n = group`order[],
		nc = group`nclasses,
		idims = group`irreducibledimensions,
		ps = Length /@ group`classes
	},
	And[
		Dimensions[group`charactertable] === {nc, nc},
	  group`charactertable[[All, 1]] === idims,
		group`charactertable[[1]] === ConstantArray[1, nc],
		And@@Flatten@Table[
		  ExpToTrig@Total[ps * Conjugate[group`charactertable[[i]]] * group`charactertable[[j]]] === n * KroneckerDelta[i, j],
			{i, nc},
			{j, i, nc}
		],
		And@@Flatten@Table[
		  ExpToTrig@Total[Conjugate[group`charactertable[[All, i]]] * group`charactertable[[All, j]]] === n / ps[[i]] * KroneckerDelta[i, j],
			{i, nc},
			{j, i, nc}
		]
	]
]


group`represent[rule_List] := group`elems // rewriteIt[
  group`matrixevaluate,
	ReplaceAll[Rule@@@rule]
];


End[]


Print[$Context];


EndPackage[]
