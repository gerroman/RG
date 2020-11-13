(* ::Package:: *)
(* Utils to present results of evaluation *)


BeginPackage["RG`Presentation`", {"RG`BaseUtils`"}];


tagged::usage = "
  tagged[eq`tag = ...] make definition for eq`tag and produce output cell with the tag \"eq`tag\"
  tagged[expr] evaluate expr and produce output cell with the tag \"expr\"
"

tagged`final = False;


colorize::usage = "
  colorize[pattern] colorize matches for the pattern
  colorize[{x1, ...}] colorize specific expressions x1, ...
";


getRunner::usage = "
  getRunner[] \[LongDash] create pallete for evaluate cells, hide/show code, and clear all outputs
";


OverTilde::usage = "
  OverTilde[func][args][expr] works as func[expr, args] i.e. it creates operator OverTilde[func][args] for the first argument of func
";


UnderBar::usage = "
  UnderBar[expr] is equivalent for HoldForm[expr]
";


hold::usage = "
  hold[pattern] apply HoldForm to matches of pattern
  hold[{x1,...}] apply HoldForm to specific xs
";


row::usage = "
  row is shortcut for Row[#, \",\t\"]&
";


Begin["`Private`"];



SetAttributes[tagged, HoldAll];

tagged::shdw = "Warning: `` appeares more than once so can shadow previous result";

Options[tagged] = {"form" -> TraditionalForm};
tagged[expr:Set[lhs_, _], args___] := (
  expr; tagged[lhs, args];
);

tagged[expr_, func_:Identity, opts:OptionsPattern[]] := (
  If[Not[$Notebooks],
    Return[expr]
  ];
  With[
    {
      tag = ToString[HoldForm[expr]],
      form = OptionValue[tagged, "form"]
    },
    CellPrint[
      ExpressionCell[expr // func // colorize[_HoldForm] // form,
      "Output", CellTags -> tag, ShowCellTags -> True, opts]
    ];
    NotebookLocate[tag];
    If[Length[SelectedCells[]] > 1,
      Message[tagged::shdw, tag],
      If[tagged`final,
        FrontEndExecute[FrontEndToken["OpenCloseGroup"]]
      ]
    ];
  ];
);
tagged[expr_, opts:OptionsPattern[]] := tagged[expr, Identity, opts];


colorize[xs_List] := With[{
    n = Length[xs]
  },
  With[{
      styles = If[n > 0, Array[i \[Function] ColorData["DarkRainbow"][i / Max[1, n - 1]], n, 0], {}]
    },
    With[{
        rules = Thread[Inner[(#1 -> Style[#1, #2])&, xs, styles, List]]
      },
      ReplaceAll[rules]
    ]
  ]
];
colorize[pattern_] := Function[expr,
  With[{
      xs = Union@Cases[{expr}, pattern, Infinity]
    },
    colorize[xs][expr]
  ]
];
colorize[xs__] := colorize[{xs}];

getRunner[] := getRunner[EvaluationNotebook[]];
getRunner[nb_NotebookObject] := CreatePalette[{
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
   Button["Show Code", (
     SetSelectedNotebook[nb];
     NotebookFind[nb, "in", All, CellTags];
     NotebookFind[nb, "Input", All, CellStyle];
     NotebookFind[nb, "in", All, CellTags];
     )],
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
   (* TODO: it does not work properly
   Button["Clear & Quit", (
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
     Quit[];
     )],*)
   Button["Print to PDF", (
     SetSelectedNotebook[nb];
     NotebookPrint[nb, Interactive->True]
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
    )]
   } // Column];


OverTilde := carryFirst;


UnderBar = HoldForm;


setAttributes[hold, HoldAll];
hold[xs_List] := With[
  {
    rules = Thread[Rule[xs, Thread[HoldForm[xs]]]]
  },
  ReplaceAll[rules]
];
hold[pattern_] := Function[expr,
  With[
    {
      xs = Union@Cases[{expr}, pattern, Infinity]
    },
    hold[xs][expr]
  ]
];
hold[xs__] := hold[{xs}];


row = Row[#, ",\t"] &;


End[];


EndPackage[];