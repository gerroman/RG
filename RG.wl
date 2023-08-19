(* ::Text:: *)
(* [RG.wl]: base script to load packages and definitions *)


(* ::Text:: *)
(* [note]: this is not a package, since in the other case autocompletion does not work *)


(* ::Text:: *)
(* [info]: setting temporary-, figure-, and working- directories*)


SetDirectory[$InitialDirectory];


temporarydirectory::usage = "temporarydirectory return location to save auxiliary files,
	[default]: '/tmp/RG/'
";
temporarydirectory = FileNameJoin[{
  $TemporaryDirectory, "RG"
}];
If[Not@FileExistsQ[temporarydirectory],
  CreateDirectory[temporarydirectory]
];
(* Print["[info]: set temporary_directory to " <> temporarydirectory]; *)

workingdirectory::usage = "workingdirectory \[LongDash] current working directory
	[default]: NotebookDirectory[] or 'temporarydirectory'
";
workingdirectory = If[$Notebooks,
  Check[NotebookDirectory[], temporarydirectory],
	temporarydirectory
] // AbsoluteFileName;
SetDirectory[workingdirectory];
(* Print["[info]: set working_directory to " <> workingdirectory]; *)


figuredirectory::usage = "
  figuredirectory return location to save figures
	[default]: NoteBookDirectory[] or 'temporarydirectory'
";
figuredirectory = If[$Notebooks, workingdirectory, temporarydirectory] // AbsoluteFileName;

(* Print["[info]: set figure directory to " <> figuredirectory]; *)


Needs /@ {
	"RG`BaseUtils`"								(* load, loadFigure, off, on, hold *)
	, "RG`Presentation`"					(* tagged, untagged, colorize, column, grid, shorten, getRunner *)
	, "RG`CommonNotation`"				(* set[Indexed, Superscript, Subscript, Prime, Bar, Hat, Tilde] *)
		 														(*, minus, plus, min, max, integrate, sum, limit, d, dt, pd, at *)
	, "RG`Calculation`"						(* modify, pullFactors, groupIt, fixedPoint, release, factorIt *)
		 														(* factorItFast, pullIt, powersPattern, changeSign, rewriteIt, *)
		 														(* toRules, toEquals, powerExpand, collectLogs, changeLogPower *)
		 														(* complexToAbs, solve, changeIntegrateVars, changeSumVars, setIntegrateLimits *)
		 														(* pullIntegrateFactors, pullSumFactors, groupIntegrals, groupSums, *)
																(* process, processList, ffirst, force, jacobian, changeVars *)
	, "RG`Diagrams`"							(* draw[Line, Arrow, Spring, Wave, Label, Frame], lineDirectives, waveParams*)
};
