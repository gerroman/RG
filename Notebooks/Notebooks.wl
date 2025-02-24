(* ::Package:: *)
BeginPackage["RG`Notebooks`"]


colorize::usage="colorize[pattern] \[LongDash] colorize matches for the pattern
colorize[{x1, ...}] \[LongDash] colorize specific expressions x1, ...
colorize[xs_List, \"StyleFunction\"->func] \[LongDash] use specific style function func[x] to colorize x, \
(default = None)
colorize[xs_List, \"ColorFunction\"->func] \[LongDash] use specific color function func[xs] for colorization of \
the whole expr (default - based on DarkRainbow color data)
colorize[xs_List|pattern, style] \[LongDash] shortcut for colorize[xs, \"StyleFunction\"->(Style[#, style]&)]
"


shorten::usage = "
  shorten[expr] print shorten version of expr
";


getRunner::usage = "
  getRunner[] \[LongDash] create pallete for evaluate cells, hide/show code, and clear all outputs
  getRunner[nb]  \[LongDash] create pallete in new window
"


getCellArchivator::usage = "getCellArchivator[] \[LongDash] create pallete to \"un(archive)\" cells within notebook"


holdform::usage = "holdform[expr] \[LongDash] replace expr -> HoldForm[expr]"


Begin["`Private`"];


Options[colorize] = {
  "ColorFunction" -> Function[{xs},
    With[{n=Length[xs]},
      With[{colors = Array[ColorData["DarkRainbow"][# / Max[1, n - 1]]&, n, 0]},
        With[{styledxs = MapIndexed[Style[#1, colors[[#2]]]&, xs]},
          ReplaceRepeated[#, Join[Thread[styledxs -> styledxs], Thread[xs->styledxs]]]&
        ]
      ]
    ]
  ],
  "StyleFunction" -> None
};
colorize[xs_List, opts:OptionsPattern[]] := Function[
  expr,
  With[{
      cf=OptionValue[colorize, "ColorFunction"],
      sf=OptionValue[colorize, "StyleFunction"]
    },
    If[sf===None,
      cf[xs][expr],
      ReplaceRepeated[expr, Join[Thread[sf/@xs->sf/@xs], Thread[xs->sf/@xs]]]
    ]
  ]
]
colorize[pattern_, opts:OptionsPattern[]]:=Function[expr,
    With[{xs=Union@Cases[{expr}, pattern, Infinity]},
      colorize[xs, opts][expr]
    ]
];
colorize[arg_, style_] := colorize[arg, "StyleFunction"->(Style[#, style]&)];


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


getCellArchivator[]:=CreatePalette[DisplayForm[GridBox[{
{
ButtonBox["Prev",
ButtonFunction:>SelectionMove[SelectedNotebook[],Previous,Cell] ,
Evaluator->Automatic
]
,
ButtonBox["Next",
ButtonFunction:>SelectionMove[SelectedNotebook[],Next,Cell],
Evaluator->Automatic
]
},
{
ButtonBox["Unarchive",
ButtonFunction:>With[{content=NotebookRead[SelectedNotebook[]]},
(
Switch[content[[2]],
"Input",SetOptions[NotebookSelection[SelectedNotebook[]],{
Evaluatable->True,
Editable->True,
CellTags->{}
}
],
_,SetOptions[NotebookSelection[SelectedNotebook[]],{
Editable->True
}
]
];
SelectionMove[SelectedNotebook[],Next,Cell];
)
],
Evaluator->Automatic
],
ButtonBox["Archive",
ButtonFunction:>With[{content=NotebookRead[SelectedNotebook[]]},
(
Switch[content[[2]],
"Input",SetOptions[NotebookSelection[SelectedNotebook[]],{
Evaluatable->False,
Editable->False,
CellTags->"archive"
}
],
_,SetOptions[NotebookSelection[SelectedNotebook[]],{
Editable->False
}
]
];
SelectionMove[SelectedNotebook[],Next,Cell];
)
],
Evaluator->Automatic
]
},
{
ButtonBox["Editable",
ButtonFunction:>(
SetOptions[NotebookSelection[SelectedNotebook[]],Editable->True];
),
Evaluator->Automatic
],
ButtonBox["Uneditable",
ButtonFunction:>(
SetOptions[NotebookSelection[SelectedNotebook[]],Editable->False];
),
Evaluator->Automatic
]
},
{
ButtonBox["Evaluatable",
ButtonFunction:>(
SetOptions[NotebookSelection[SelectedNotebook[]],Evaluatable->True];
),
Evaluator->Automatic
],
ButtonBox["Unevaluatable",
ButtonFunction:>(
SetOptions[NotebookSelection[SelectedNotebook[]],Evaluatable->False];
),
Evaluator->Automatic
]
}
}]
],WindowMargins->{{0, Automatic}, {Automatic, 0}}, WindowTitle->"(Un)archive cells ..."];


setDrawOptions[] := Get["RG/Tools/SetDrawOptions.wl"];


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



Quiet@Needs["RG`Scripts`", FileNameJoin[{"RG","Tools","Scripts.wl"}]];
Needs["RG`Tools`"];

Print["[info]: " <> RG`Scripts`Private`systemString];
Print["[date]: " <> RG`Scripts`Private`timeString];
Print[
  ToString@StringForm["[git]: `` - [RG-package]",
    RG`Scripts`gitRef[FileNameJoin[{$UserBaseDirectory, "Applications", "RG"}]]
  ]
];
If[$Notebooks, Print[
  ToString@StringForm["[git]: `` - [notebook]",
    RG`Scripts`gitRef[NotebookDirectory[]]
  ]
]];
