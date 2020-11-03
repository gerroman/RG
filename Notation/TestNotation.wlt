BeginTestSection["TestNotation"]


Needs["RG`Notation`"];
tf = TraditionalForm;

VerificationTest[(* 1 *)
	CompoundExpression[RG`Notation`setIndexed[x], ExportString[tf[x[1]], "TeXFragment"]]
	,
	"\\[x_1\\]\n\n"	
]

VerificationTest[(* 2 *)
	CompoundExpression[RG`Notation`setPrime[x], ExportString[tf[prime`x], "TeXFragment"]]
	,
	"\\[x'\\]\n\n"	
]

VerificationTest[(* 3 *)
	CompoundExpression[RG`Notation`setBar[x], ExportString[tf[bar`x], "TeXFragment"]]
	,
	"\\[\\bar{x}\\]\n\n"	
]

VerificationTest[(* 4 *)
	ExportString[tf[RG`Notation`matrixElement["ann"]], "TeXFragment"]
	,
	"\\[\\mathcal{M}_{\\text{ann}}\\]\n\n"	
]

EndTestSection[]
