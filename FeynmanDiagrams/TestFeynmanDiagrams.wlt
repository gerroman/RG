BeginTestSection["TestFeynmanDiagrams"]

Needs["RG`FeynmanDiagrams`"];

VerificationTest[(* 1 *)
	Hash[Function[ExportString[Slot[1], "GIF"]][RG`FeynmanDiagrams`photonLine[List[List[0, 0], List[1, 0]], q, 4]]]
	,
	1545370226565320586	
]

VerificationTest[(* 2 *)
	Hash[Function[ExportString[Slot[1], "GIF"]][RG`FeynmanDiagrams`electronLine[List[List[-2, 0], List[0, 0], List[2, 1]], List[p, Derivative[1][p]]]]]
	,
	462923480664741489	
]

VerificationTest[(* 3 *)
	Hash[Function[ExportString[Slot[1], "GIF"]][RG`FeynmanDiagrams`photonArc[List[List[-1, 1], List[1, 1]], k, 10]]]
	,
	2176285551862831065	
]

VerificationTest[(* 4 *)
	Hash[Function[ExportString[Slot[1], "GIF"]][RG`FeynmanDiagrams`electronArc[List[List[-1, 1], List[1, 1]], p]]]
	,
	2174330150491176586	
]

EndTestSection[]
