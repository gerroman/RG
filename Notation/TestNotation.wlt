Needs["RG`Notation`"];


test[1] := VerificationTest[(* #1 *)
	CompoundExpression[setIndexed[x], ExportString[TraditionalForm[x[1]], "TeXFragment"]]
	,
	"\\[x_1\\]\n\n"	
]

test[2] := VerificationTest[(* #2 *)
	CompoundExpression[setPrime[x], ExportString[TraditionalForm[prime`x], "TeXFragment"]]
	,
	"\\[x'\\]\n\n"	
]

test[3] := VerificationTest[(* #3 *)
	CompoundExpression[setBar[x], ExportString[TraditionalForm[bar`x], "TeXFragment"]]
	,
	"\\[\\bar{x}\\]\n\n"	
]

test[4] := VerificationTest[(* #4 *)
	ExportString[TraditionalForm[matrixElement["ann"]], "TeXFragment"]
	,
	"\\[\\mathcal{M}_{\\text{ann}}\\]\n\n"	
]


test[5] := VerificationTest[(* #5 *)
  ExportString[TraditionalForm[mass[e]], "TeXFragment"]
  ,
  "\\[m_e\\]\n\n"	
]


test[6] := VerificationTest[(* #6 *)
  ExportString[TraditionalForm[energy[p]], "TeXFragment"]
  ,
  "\\[p^0\\]\n\n"	
]

test[7] := VerificationTest[(* #7 *)
  ExportString[TraditionalForm[sp[p]], "TeXFragment"]
  ,
  "\\[p^2\\]\n\n"	
]

test[8] := VerificationTest[(* #8 *)
  ExportString[TraditionalForm[sp[p, p]], "TeXFragment"]
  ,
  "\\[p^2\\]\n\n"	
]

test[9] := VerificationTest[(* #9 *)
  ExportString[TraditionalForm[sp[a, b]], "TeXFragment"]
  ,
  "\\[\\text{}(a,b)\\]\n\n"	
]

test[10] := VerificationTest[(* #10 *)
  ExportString[TraditionalForm[abs[x]], "TeXFragment"]
  ,
  "\\[\\left| x\\right|\\]\n\n"	
]


test[11] := VerificationTest[(* #11 *)
  (setLorentzIndex[\[Rho], \[Nu]]; ExportString[TraditionalForm[sp[\[Rho], \[Nu]]], "TeXFragment"])
  ,
   "\\[g^{\\rho \\nu }\\]\n\n"
]

test[12] := VerificationTest[(* #12 *)
  (setLorentzIndex[\[Rho], \[Nu]]; ExportString[TraditionalForm[sp[a, \[Nu]]], "TeXFragment"])
  ,
   "\\[a^{\\nu }\\]\n\n"
]

test[13] := VerificationTest[(* #13 *)
  (setLorentzIndex[\[Rho], \[Nu]]; ExportString[TraditionalForm[sp[\[Nu], a + b]], "TeXFragment"])
  ,
  "\\[\\langle a+b\\rangle ^{\\nu }\\]\n\n"
]


test[14] := VerificationTest[(* #14 *)
  ExportString[TraditionalForm[\[Gamma][\[Rho]]], "TeXFragment"]
  ,
  "\\[\\gamma ^{\\rho }\\]\n\n"
]


test[15] := VerificationTest[(* #15 *)
  ExportString[TraditionalForm[\[Gamma][p+q]], "TeXFragment"]
  ,
  "\\[\\gamma ^{p+q}\\]\n\n"
]

test[16] := VerificationTest[(* #16 *)
  ExportString[TraditionalForm[m id], "TeXFragment"]
  ,
  "\\[m\\]\n\n"
]

test[17] := VerificationTest[(* #17 *)
  ExportString[TraditionalForm[id], "TeXFragment"]
  ,
  "\\[1\\]\n\n"
]

test[18] := VerificationTest[(* #18 *)
  ExportString[TraditionalForm[theta], "TeXFragment"]
  ,
  "\\[\\theta\\]\n\n"
]

test[19] := VerificationTest[(* #19 *)
  ExportString[TraditionalForm[phi], "TeXFragment"]
  ,
  "\\[\\varphi\\]\n\n"
]

test[20] := VerificationTest[(* #20 *)
  ExportString[TraditionalForm[omega], "TeXFragment"]
  ,
  "\\[\\Omega\\]\n\n"
]

test[21] := VerificationTest[(* #21 *)
  ExportString[TraditionalForm[bar`u], "TeXFragment"]
  ,
  "\\[\\bar{u}\\]\n\n"	
]

test[22] := VerificationTest[(* #22 *)
  ExportString[TraditionalForm[bar`v], "TeXFragment"]
  ,
  "\\[\\bar{v}\\]\n\n"	
]

test[23] := VerificationTest[(* #23 *)
  setPrime[\[Rho]]
  ,
  prime`\[Rho]
]

test[24] := VerificationTest[(* #24 *)
  setBar[\[Rho]]
  ,
  bar`\[Rho]
]

test[25] := VerificationTest[(* #25 *)
  setLorentzIndex[a]
  ,
  {\[Rho], prime`\[Rho], \[Nu], prime`\[Nu], a, prime`a}
]

test[26] := VerificationTest[(* #26 *)
  ExportString[TraditionalForm[d[r]/d[x]], "TeXFragment"]
  ,
  "\\[\\frac{dr}{dx}\\]\n\n"	
]

test[27] := VerificationTest[(* #27 *)
  ExportString[TraditionalForm[pcms], "TeXFragment"]
  ,
  "\\[\\mathit{p}_{\\text{cms}}\\]\n\n"
]


test[28] := VerificationTest[(* #28 *)
  ExportString[TraditionalForm[prime`pcms], "TeXFragment"]
  ,
  "\\[\\mathit{p}_{\\text{cms}}{}^{\\prime }\\]\n\n"
]

test[29] := VerificationTest[(* #29 *)
  ExportString[TraditionalForm/@{electron, positron, muon, antimuon, photon}, "TeXFragment"]
  ,
  "\\[\\left\\{e,\\bar{e},\\mu ,\\bar{\\mu },\\gamma \\right\\}\\]\n\n"
]


test[30] :=
VerificationTest[(* #30 *)
  Block[{i, x},
    setSuperscript[x];
    ExportString[TraditionalForm[x[i]], "TeXFragment"]
  ]
  ,
  "\\[x^i\\]\n\n"
]


test[31] :=
VerificationTest[(* #31 *)
  Block[{x},
	  setTilde[x];
		ExportString[TraditionalForm[tilde`x], "TeXFragment"]
	]
  ,
  "\\[\\tilde{x}\\]\n\n"
]
