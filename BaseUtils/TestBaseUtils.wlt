Needs["RG`BaseUtils`"];

test[1] := VerificationTest[(* #1 *)
  a - b - 2 c // hold[a, b, c]
	,
	HoldForm[a] - HoldForm[b] - 2 HoldForm[c]
]

test[2] := VerificationTest[(* #2 *)
  a - 3 a - 2 c // hold[a]
	,
	-2 HoldForm[a] - 2 c
]
