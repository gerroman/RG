(* ::Package:: *)

(* ::Title:: *)
(*Access to PDG data*)


BeginPackage["RG`PDG`", {"RG`BaseUtils`"}]


pdglinks::usage = "
  pdglinks return a list of links to PDG pdfs   
"

pdg::usage = "
  pdg return a list of pdgnames which has links to PDG pdfs
"

getpdg::usage = "
  getpdg[pdgname] download pdf for pdgname 
"


Begin["`Private`"]


getURLs[responce_, ext_:""] := Select[
  StringSplit[responce, {"\n","<",">"," ",","}], 
  StringMatchQ[#, "href="~~__~~ext~~___]&
]

load["pdglinks.mx", pdglinks,
  pdglinks = getURLs[URLFetch["https://pdg.lbl.gov/2021/listings/contents_listings.html"], "pdf"]
]

load["pdg.mx", pdg,
  pdg = Select[pdglinks, StringMatchQ[#, __~~"-list-"~~__]&] // 
  StringReplace[#, __~~"-list-" -> ""]& // 
  StringReplace[#, ".pdf"~~___->""]&
]


getpdg[name_String] := With[
  {fname = FileNameJoin[{temporary\[LetterSpace]directory, name<>".pdf"}]},
  If[FileExistsQ[fname],
    (Print["File "<>fname<>" does exists"]; fname),
    With[{url="https://pdg.lbl.gov/2021/listings/rpp2021-list-"<>name<>".pdf"},
      Print["Downloading ... ", url];
      URLSave[url, fname]
    ]
  ]
] 


End[]


EndPackage[]
