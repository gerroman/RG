BeginTestSection["TestNotation"]


Needs["RG`Notation`"];


VerificationTest[(* #1 *)
	CompoundExpression[setIndexed[x], ExportString[TraditionalForm[x[1]], "TeXFragment"]]
	,
	"\\[x_1\\]\n\n"	
]

VerificationTest[(* #2 *)
	CompoundExpression[setPrime[x], ExportString[TraditionalForm[prime`x], "TeXFragment"]]
	,
	"\\[x'\\]\n\n"	
]

VerificationTest[(* #3 *)
	CompoundExpression[setBar[x], ExportString[TraditionalForm[bar`x], "TeXFragment"]]
	,
	"\\[\\bar{x}\\]\n\n"	
]

VerificationTest[(* #4 *)
	ExportString[TraditionalForm[matrixElement["ann"]], "TeXFragment"]
	,
	"\\[\\mathcal{M}_{\\text{ann}}\\]\n\n"	
]


VerificationTest[(* #5 *)
  ExportString[TraditionalForm[mass[e]], "TeXFragment"]
  ,
  "\\[m_e\\]\n\n"	
]


VerificationTest[(* #6 *)
  ExportString[TraditionalForm[energy[p]], "TeXFragment"]
  ,
  "\\[p^0\\]\n\n"	
]

VerificationTest[(* #7 *)
  ExportString[TraditionalForm[sp[p]], "TeXFragment"]
  ,
  "\\[p^2\\]\n\n"	
]

VerificationTest[(* #8 *)
  ExportString[TraditionalForm[sp[p, p]], "TeXFragment"]
  ,
  "\\[p^2\\]\n\n"	
]

VerificationTest[(* #9 *)
  ExportString[TraditionalForm[sp[a, b]], "TeXFragment"]
  ,
  "\\[\\text{}(a,b)\\]\n\n"	
]

VerificationTest[(* #10 *)
  ExportString[TraditionalForm[abs[x]], "TeXFragment"]
  ,
  "\\[\\left| x\\right|\\]\n\n"	
]


VerificationTest[(* #11 *)
  (setLorentzIndex[\[Rho], \[Nu]]; ExportString[TraditionalForm[sp[\[Rho], \[Nu]]], "TeXFragment"])
  ,
   "\\[g^{\\rho \\nu }\\]\n\n"
]

VerificationTest[(* #12 *)
  (setLorentzIndex[\[Rho], \[Nu]]; ExportString[TraditionalForm[sp[a, \[Nu]]], "TeXFragment"])
  ,
   "\\[a^{\\nu }\\]\n\n"
]

VerificationTest[(* #13 *)
  (setLorentzIndex[\[Rho], \[Nu]]; ExportString[TraditionalForm[sp[\[Nu], a + b]], "TeXFragment"])
  ,
  "\\[\\langle a+b\\rangle ^{\\nu }\\]\n\n"
]


VerificationTest[(* #14 *)
  ExportString[TraditionalForm[\[Gamma][\[Rho]]], "TeXFragment"]
  ,
  "\\[\\gamma ^{\\rho }\\]\n\n"
]


VerificationTest[(* #15 *)
  ExportString[TraditionalForm[\[Gamma][p+q]], "TeXFragment"]
  ,
  "\\[\\gamma ^{p+q}\\]\n\n"
]

VerificationTest[(* #16 *)
  ExportString[TraditionalForm[m id], "TeXFragment"]
  ,
  "\\[m\\]\n\n"
]

VerificationTest[(* #17 *)
  ExportString[TraditionalForm[id], "TeXFragment"]
  ,
  "\\[1\\]\n\n"
]

VerificationTest[(* #18 *)
  ExportString[TraditionalForm[theta], "TeXFragment"]
  ,
  "\\[\\theta\\]\n\n"
]

VerificationTest[(* #19 *)
  ExportString[TraditionalForm[phi], "TeXFragment"]
  ,
  "\\[\\varphi\\]\n\n"
]

VerificationTest[(* #20 *)
  ExportString[TraditionalForm[omega], "TeXFragment"]
  ,
  "\\[\\Omega\\]\n\n"
]

VerificationTest[(* #21 *)
  ExportString[TraditionalForm[bar`u], "TeXFragment"]
  ,
  "\\[\\bar{u}\\]\n\n"	
]

VerificationTest[(* #22 *)
  ExportString[TraditionalForm[bar`v], "TeXFragment"]
  ,
  "\\[\\bar{v}\\]\n\n"	
]

VerificationTest[(* #23 *)
  setPrime[\[Rho]]
  ,
  prime`\[Rho]
]

VerificationTest[(* #24 *)
  setBar[\[Rho]]
  ,
  bar`\[Rho]
]

VerificationTest[(* #25 *)
  setLorentzIndex[a]
  ,
  {\[Rho], prime`\[Rho], \[Nu], prime`\[Nu], a, prime`a}
]

VerificationTest[(* #26 *)
  ExportString[TraditionalForm[d[r]/d[x]], "TeXFragment"]
  ,
  "\\[\\frac{dr}{dx}\\]\n\n"	
]

VerificationTest[(* #27 *)
  ExportString[TraditionalForm[pcms], "TeXFragment"]
  ,
  "\\[\\mathit{p}_{\\text{cms}}\\]\n\n"
]


VerificationTest[(* #28 *)
  ExportString[TraditionalForm[prime`pcms], "TeXFragment"]
  ,
  "\\[\\mathit{p}_{\\text{cms}}{}^{\\prime }\\]\n\n"
]

VerificationTest[(* #29 *)
  ExportString[TraditionalForm/@{electron, positron, muon, antimuon, photon}, "TeXFragment"]
  ,
  "\\[\\left\\{e,\\bar{e},\\mu ,\\bar{\\mu },\\gamma \\right\\}\\]\n\n"
]

EndTestSection[]
