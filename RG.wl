
(* Temporary directory 												   :temp: *)

temporarydirectory::usage = "temporarydirectory - location to save auxiliary files,
    [default]: '/tmp/RG/'
";
temporarydirectory = FileNameJoin[{$TemporaryDirectory, "RG"}];

(* Working directory													   :work: *)

workingdirectory::usage = "workingdirectory \[LongDash] current working directory
    [default]: NotebookDirectory[] or 'temporarydirectory'
";
workingdirectory = AbsoluteFileName[If[$Notebooks, Check[NotebookDirectory[], temporarydirectory], temporarydirectory]];

(* Figure directory													 :figure: *)

figuredirectory::usage = "figuredirectory return location to save figures
    [default]: NoteBookDirectory[] or 'temporarydirectory'
";
figuredirectory = AbsoluteFileName[If[$Notebooks, workingdirectory, temporarydirectory]];

(* Packages														   :packages: *)

If[Not@FileExistsQ[temporarydirectory], CreateDirectory[temporarydirectory]];
SetDirectory[workingdirectory];

Needs["RG`BaseUtils`"];
Needs["RG`Presentation`"];
Needs["RG`CommonNotation`"];
Needs["RG`Calculation`"];
Needs["RG`Diagrams`"];
