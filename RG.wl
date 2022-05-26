(* ::Text:: *)
(*RG.wl: base file to load all subpackages and definitions *)

(*
  [NOTE]: [!] this is not a package, since in another case autocompletion does not work
*)


(* ::Text:: *)
(*Environment: temporary-, figure-, and working- directories*)


temporarydirectory::usage = "
  temporarydirectory return location to save auxiliary files
";

figuredirectory::usage = "
  figuredirectory return location to save figures
";

workingdirectory::usage = "
  workingdirectory \[LongDash] current working directory
";


temporarydirectory = FileNameJoin[{$TemporaryDirectory, "RG"}];
If[Not[FileExistsQ[temporarydirectory]], CreateDirectory[temporarydirectory]];
Echo["[Info]: set temporary directory to " <> temporarydirectory];
workingdirectory = If[$Notebooks,
  Check[NotebookDirectory[], temporarydirectory],
	temporarydirectory
];
Echo["[Info]: set working directory to " <> workingdirectory];
SetDirectory[workingdirectory];
figuredirectory = workingdirectory;
Echo["[Info]: set figure directory to " <> figuredirectory];


Needs /@ {
	"RG`BaseUtils`"
	, "RG`Presentation`"
	, "RG`CommonNotation`"
	, "RG`Calculation`"
	(* , "RG`Notation`" *)
	(* , "RG`Kinematics`" *)
	(* , "RG`Traces`" *)
	(* , "RG`FeynmanDiagrams`" *)
	(* , "RG`Particles`" *)
	, "RG`Diagrams`"
	(* , "RG`HelicityStates`" *)
};
