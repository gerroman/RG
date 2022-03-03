(* ::Text:: *)
(*RG.wl: base file to load all subpackages and definitions *)

(*
  [NOTE]: it is not a package, because in another case autocompletion does not work
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

