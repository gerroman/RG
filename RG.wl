(* ::Text:: *)
(*RG.wl: base file to load all subpackages and definitions *)

(*
  [NOTE]: [!] this is not a package, since in another case autocompletion does not work
*)

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
