(* ::Text:: *)
(*RG.wl: base file to load all subpackages and definitions *)

(*[NOTE]: this is not a package since autocompletion does not work*)

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

workingdirectory::usage = "
  workingdirectory \[LongDash] current working directory
";

workingdirectory = Check[NotebookDirectory[], $InitialDirectory];

Echo["[Info]: Set working directory to " <> workingdirectory];
SetDirectory[workingdirectory];
