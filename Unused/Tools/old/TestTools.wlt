BeginTestSection["TestTools"]

VerificationTest[(* 1 *)
	Needs["RG`Tools`"]
	,
	Null	
	,
	TestID->"9a5372e3-f639-40b5-8f5f-d4752f55d6ae"
]

BeginTestSection["Kernel working and logging"]

VerificationTest[(* 2 *)
	RG`Tools`print["test"]
	,
	Null	
	,
	TestID->"1d9354a9-a40f-40a5-969b-d295bc1e944c"
]

VerificationTest[(* 3 *)
	RG`Tools`pprint["test"]
	,
	Null	
	,
	TestID->"318b85ad-a83c-4ecc-8876-1ea91e5547bf"
]

VerificationTest[(* 4 *)
	RG`Tools`info[RG`Tools`info]
	,
	Null	
	,
	TestID->"5f267437-695b-4770-bdee-8c4788d5c415"
]

EndTestSection[]

BeginTestSection["FrontEnd operations from scripts"]

VerificationTest[(* 5 *)
	MatchQ[RG`Tools`installFrontEnd[], List[BlankNullSequence[LinkObject]]]
	,
	True	
	,
	TestID->"01ebe7ad-9a2b-4745-bd5c-34267e3b3b55"
]

VerificationTest[(* 6 *)
	MatchQ[RG`Tools`uninstallFrontEnd[], List[BlankNullSequence[LinkObject]]]
	,
	True	
	,
	TestID->"8e91cd6e-4fe2-4c73-8a68-4fefd3ad6802"
]

VerificationTest[(* 7 *)
	If[$Notebooks, MatchQ[RG`Tools`show["test"], Blank[NotebookObject]], False]
	,
	True	
	,
	TestID->"ef090fa0-f85a-49f8-ba2c-74ddf4ae8c5d"
]

EndTestSection[]

BeginTestSection["Equations"]

VerificationTest[(* 8 *)
	RG`Tools`present[x]
	,
	Equal[HoldForm[x], x]	
	,
	TestID->"e97f3f8e-f44a-427e-9203-8f35f9139ca8"
]

VerificationTest[(* 9 *)
	RG`Tools`processList[ReplaceAll[Rule[x, y]], ReplaceAll[Rule[y, z]]][x]
	,
	List[x, y, z]	
	,
	TestID->"bdc5c274-d698-48f3-8a1e-34443607927c"
]

VerificationTest[(* 10 *)
	RG`Tools`process[ReplaceAll[Rule[x, y]], ReplaceAll[Rule[y, z]]][x]
	,
	List[x, z]	
	,
	TestID->"dbf1cc13-f1a6-4a4d-b4bc-e4a58fac5a20"
]

VerificationTest[(* 11 *)
	RG`Tools`rewriteIt[List[ReplaceAll[Rule[x, z]]], List[ReplaceAll[Rule[y, t]]]][Equal[x, y]]
	,
	Equal[z, t]	
	,
	TestID->"53287193-ef8e-437b-a210-ef26951144e8"
]

VerificationTest[(* 12 *)
	RG`Tools`rewriteIt[List[ReplaceAll[Rule[x, z]]], List[ReplaceAll[Rule[y, t]]]][Times[x, y]]
	,
	Equal[Times[y, z], Times[t, x]]	
	,
	TestID->"f5ea9902-4d07-43de-8e5c-c4ad24e62751"
]

EndTestSection[]

BeginTestSection["Equations"]

VerificationTest[(* 13 *)
	RG`Tools`ffirst[List[List[1, 2], 3]]
	,
	1
	,
	{ffirst::argx}
	,
	TestID->"fb55a9e8-9696-4603-bc6c-48bcf5fcca56"
]

VerificationTest[(* 14 *)
	RG`Tools`ffirst[List[List[1, 2], 3], Rule["verbose", False]]
	,
	1	
	,
	TestID->"ff297185-6514-4978-926e-920a34995577"
]

VerificationTest[(* 15 *)
	RG`Tools`flast[List[List[1, 2], 3, List[4]]]
	,
	4
	,
	{flast::argx}
	,
	TestID->"cb36a50c-3d93-48ae-bcff-3abfd6b4541f"
]

VerificationTest[(* 16 *)
	RG`Tools`flast[List[List[1, 2], 3, List[4]], Rule["verbose", False]]
	,
	4	
	,
	TestID->"97f61671-8ba1-450d-8040-603e1214bc14"
]

VerificationTest[(* 17 *)
	RG`Tools`emptyQ[List[1, 2, 3]]
	,
	False	
	,
	TestID->"3ee5bbb4-88db-43d2-b3ff-1126318fdce8"
]

VerificationTest[(* 18 *)
	RG`Tools`emptyQ[List[]]
	,
	True	
	,
	TestID->"8da936c4-7612-4643-bb91-0b5d5a2efc6b"
]

EndTestSection[]

EndTestSection[]
