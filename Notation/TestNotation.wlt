BeginTestSection["TestNotation"]


Needs["RG`Notation`"];

VerificationTest[(* 1 *)
	CompoundExpression[setIndexed[x], ExportString[TraditionalForm[x[1]], "TeXFragment"]]
	,
	"\\[x_1\\]\n\n"	
]

VerificationTest[(* 2 *)
	CompoundExpression[setPrime[x], ExportString[TraditionalForm[prime`x], "TeXFragment"]]
	,
	"\\[x'\\]\n\n"	
]

VerificationTest[(* 3 *)
	CompoundExpression[setBar[x], ExportString[TraditionalForm[bar`x], "TeXFragment"]]
	,
	"\\[\\bar{x}\\]\n\n"	
]

VerificationTest[(* 4 *)
	ExportString[TraditionalForm[matrixElement["ann"]], "TeXFragment"]
	,
	"\\[\\mathcal{M}_{\\text{ann}}\\]\n\n"	
]


VerificationTest[
  ExportString[TraditionalForm[mass[e]], "TeXFragment"]
  ,
  "\\[m_e\\]\n\n"	
]


VerificationTest[
  ExportString[TraditionalForm[energy[p]], "TeXFragment"]
  ,
  "\\[p^0\\]\n\n"	
]

VerificationTest[
  ExportString[TraditionalForm[sp[p]], "TeXFragment"]
  ,
  "\\[p^2\\]\n\n"	
]

VerificationTest[
  ExportString[TraditionalForm[sp[p, p]], "TeXFragment"]
  ,
  "\\[p^2\\]\n\n"	
]

VerificationTest[
  ExportString[TraditionalForm[sp[a, b]], "TeXFragment"]
  ,
  "\\[\\text{}(a,b)\\]\n\n"	
]

VerificationTest[
  ExportString[TraditionalForm[abs[x]], "TeXFragment"]
  ,
  "\\[\\left| x\\right|\\]\n\n"	
]


VerificationTest[
  (setLorentzIndex[\[Mu], \[Nu]]; ExportString[TraditionalForm[sp[\[Mu], \[Nu]]], "TeXFragment"])
  ,
   "\\[g^{\\mu \\nu }\\]\n\n"
]

VerificationTest[
  (setLorentzIndex[\[Mu], \[Nu]]; ExportString[TraditionalForm[sp[a, \[Nu]]], "TeXFragment"])
  ,
   "\\[a^{\\nu }\\]\n\n"
]

VerificationTest[
  (setLorentzIndex[\[Mu], \[Nu]]; ExportString[TraditionalForm[sp[\[Nu], a + b]], "TeXFragment"])
  ,
  "\\[\\langle a+b\\rangle ^{\\nu }\\]\n\n"
]


VerificationTest[
  ExportString[TraditionalForm[\[Gamma][\[Mu]]], "TeXFragment"]
  ,
  "\\[\\gamma ^{\\mu }\\]\n\n"
]


VerificationTest[
  ExportString[TraditionalForm[\[Gamma][p+q]], "TeXFragment"]
  ,
  "\\[\\gamma ^{p+q}\\]\n\n"
]

VerificationTest[
  ExportString[TraditionalForm[m id], "TeXFragment"]
  ,
  "\\[m\\]\n\n"
]

VerificationTest[
  ExportString[TraditionalForm[id], "TeXFragment"]
  ,
  "\\[1\\]\n\n"
]

VerificationTest[
  ExportString[TraditionalForm[theta], "TeXFragment"]
  ,
  "\\[\\theta\\]\n\n"
]

VerificationTest[
  ExportString[TraditionalForm[phi], "TeXFragment"]
  ,
  "\\[\\varphi\\]\n\n"
]

VerificationTest[
  ExportString[TraditionalForm[omega], "TeXFragment"]
  ,
  "\\[\\Omega\\]\n\n"
]
EndTestSection[]
