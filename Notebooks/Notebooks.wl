(* ::Title:: *)
(*Presentation*)
Needs["RG`Tools`"];


BeginPackage["RG`Notebooks`"]


colorize::usage = "
  colorize[pattern] colorize matches for the pattern
  colorize[{x1, ...}] colorize specific expressions x1, ...
"


shorten::usage = "
  shorten[expr] print shorten version of expr
";


getRunner::usage = "
  getRunner[] \[LongDash] create pallete for evaluate cells, hide/show code, and clear all outputs
	getRunner[nb]  \[LongDash] create pallete in new window
"


holdform::usage = "holdform[expr] \[LongDash] replace expr -> HoldForm[expr]"


Begin["`Private`"];


colorize[xs_List] := With[{n = Length[xs]},
  With[{styles = If[n > 0, Array[i \[Function] ColorData["DarkRainbow"][i / Max[1, n - 1]], n, 0], {}]},
    With[{rules = Thread[Inner[(#1 -> Style[#1, #2(*, Background->Lighter@Lighter[#2]*)])&, xs, styles, List]]},
      ReplaceAll[rules]
    ]
  ]
];
colorize[pattern_] := Function[expr,
  With[{xs = Union@Cases[{expr}, pattern, Infinity]},
    colorize[xs][expr]
  ]
];
colorize[xs__] := colorize[{xs}];


SetAttributes[buttons, HoldAll];
buttons[notebook_] := With[{nb=Hold[notebook]}, Grid[{{
   Button["Evaluate Cells", (
     SetSelectedNotebook[nb];
     NotebookFind[nb, "in", All, CellTags];
     FrontEndExecute[
       FrontEndToken[nb, "ExpandSelection"]
     ];
     FrontEndExecute[
       FrontEndToken[nb, "SelectionOpenAllGroups"]
     ];
     FrontEndExecute[
       FrontEndToken[nb, "EvaluateCells"]
     ];
     NotebookFind[nb, "in", All, CellTags];
     )],
   Button["Hide Code", {
     SetSelectedNotebook[nb];
     NotebookFind[nb, "in", All, CellTags];
     NotebookFind[nb, "Output", All, CellStyle];
     FrontEndExecute[
      FrontEndToken[nb, "SelectionCloseUnselectedCells"]
     ];
     NotebookFind[nb, "in", All, CellTags];
     }],
   Button["Print to PDF", (
     SetSelectedNotebook[nb];
     NotebookPrint[nb, Interactive->True]
   )],
   Button["Select Input",
     SetSelectedNotebook[nb];
	   NotebookFind[nb, "Input", All, CellStyle];
   ],
   Button["Save",
     FrontEndTokenExecute[nb, "Save"]
   ]},
   {
   Button["Clear All Outputs", (
     SetSelectedNotebook[nb];
     NotebookFind[nb, "in", All, CellTags];
     FrontEndExecute[
       FrontEndToken[nb, "ExpandSelection"]
     ];
     FrontEndExecute[
       FrontEndToken[nb, "SelectionOpenAllGroups"]
     ];
     FrontEndExecute[
       FrontEndToken[nb, "DeleteGeneratedCells"]
     ];
     NotebookFind[nb, "in", All, CellTags];
   )],
   Button["Show Code", (
     SetSelectedNotebook[nb];
     NotebookFind[nb, "in", All, CellTags];
     NotebookFind[nb, "Input", All, CellStyle];
     NotebookFind[nb, "in", All, CellTags];
   )],
   Button["Get handouts", (
      Module[
        {
          nb2 = CreateDocument[{TextCell["Handouts", "Title"]}],
          temp = CreateTemporary[]
        },
        temp = RenameFile[temp, temp <> ".nb"];
        NotebookSave[nb2, temp]
        SetSelectedNotebook[nb2];
      ]
    )],
   Button["Select Code",
     SetSelectedNotebook[nb];
	   NotebookFind[nb, "Code", All, CellStyle];
   ],
   Button["Quit", (
       If[nb =!= EvaluationNotebook[],
          SetSelectedNotebook[nb];
          NotebookClose[ButtonNotebook[]];
       ];
       Exit[0];
     )]
  }} // Transpose, Spacings -> {0, 0}]
];


getRunner[] := CellPrint[
  ExpressionCell[ReleaseHold[buttons[EvaluationNotebook[]]],
    "Text", CellTags->"run", ShowCellTags -> True, GeneratedCell->False
  ]
];


getRunner[nb_NotebookObject] := CreateWindow[
  PaletteNotebook[ReleaseHold[buttons[nb]]],
  WindowTitle -> "Run_" <> Last[FileNameSplit[NotebookFileName[nb]]],
  WindowFloating -> True
];


Options[shorten] = {"n" -> 1, "HoldForm" -> False};
shorten[x_String, opts : OptionsPattern[]] := x;
shorten[empty:{}, opts : OptionsPattern[]] := empty;
shorten[(head_)[xs__], opts : OptionsPattern[]] := With[{
    str = ToString[StringForm["[other terms (``/``)]", Length[{xs}] - OptionValue["n"], Length[{xs}]]],
    func = If[OptionValue["HoldForm"], HoldForm, Identity],
    xn = {xs}[[;; Min[OptionValue["n"], Length[{xs}]]]]
  },
  If[Length[{xs}] <= OptionValue["n"], head[xs],
    With[{args=func /@ xn // Append[str]}, head@@args]
  ]
];
shorten[expr_, OptionsPattern[]] := expr;


SetAttributes[holdform, HoldAll]
holdform[xs__] := ReplaceRepeated[#, rule`holdform[xs]]&


End[]


EndPackage[]


Begin["rule`"];
rule`holdform::usage = "rule`holdform[expr] substitute expr -> HoldForm[expr]"
Begin["`Private`"]
SetAttributes[rule`holdform, HoldAll]
rule`holdform[x_] := {ex_HoldForm :> ex, x -> HoldForm[x]}
rule`holdform[xs__] := Prepend[Thread[{xs} -> Thread[HoldForm[{xs}]]], ex_HoldForm :> ex]
End[];
End[];


Print[ToString@StringForm["[info]: '``' loaded", $InputFileName]]
