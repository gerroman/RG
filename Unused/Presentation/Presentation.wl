(* ::Package:: *)

(* ::Title:: *)
(*Presentation*)


BeginPackage["RG`Presentation`", {"RG`Tools`", "RG`BaseUtils`"}];


tagged::usage = "
  tagged[eq`tag = ...] make definition for eq`tag and produce output cell with the tag \"eq`tag\"
  tagged[expr] evaluate expr and produce output cell with the tag \"expr\"
";


untagged::usage = "
  just present expression in traditional form
";


UnderBar::usage = "
  UnderBar[expr] is equivalent for HoldForm[expr]
";


colorize::usage = "
  colorize[pattern] colorize matches for the pattern
  colorize[{x1, ...}] colorize specific expressions x1, ...
";


row::usage = "
  row is shortcut for Row[#, \",\t\"]&
";


column::usage = "
  column is shortcut for Column[#, Spacings->1.5]&
"


grid::usage = "
  grid[list] decorate list as grid
";


shorten::usage = "
  shorten[expr] print shorten version of expr
";


getRunner::usage = "
  getRunner[] \[LongDash] create pallete for evaluate cells, hide/show code, and clear all outputs
	getRunner[nb]  \[LongDash] create pallete in new window
";


matrixComplexityPlot::usage = "matrixComplexityPlot[m, label] create a plot of matrix element complexity Log[10, 1 + LeafCounts[matrix elements]]";


matrixPlot::usage = "matrixPlot[m] \[LongDash] print matrix structure, indicating zero/nonzero elements"


Begin["`Private`"];


Options[tagged] = {"form" -> TraditionalForm, "colorize" -> RG`Tools`$Colorize, "final" -> False};
Options[untagged] = {"form" -> TraditionalForm, "colorize" -> RG`Tools`$Colorize, "final" -> False};


SetAttributes[tagged, HoldFirst];
tagged::shdw = "Warning: `` appeares more than once, so it can shadow the previous result";
tagged[expr_, opts:OptionsPattern[]] := tagged[expr, Identity, opts];

tagged[expr:Set[lhs_, _], args___] := (
  expr;
  tagged[lhs, args];
);

tagged[expr:present[lhs_], func_, opts:OptionsPattern[]] := With[
  {tag = ToString[HoldForm[lhs]]},
  If[Not[$Notebooks],
		TeXPrint[func[expr], tag]
		,
	  (
      CellPrint[ExpressionCell[
        expr // func //
          If[OptionValue[tagged, "colorize"], colorize[_HoldForm], Identity] //
          OptionValue[tagged, "form"],
        "Output", CellTags -> tag, ShowCellTags -> True, opts
      ]];
      NotebookLocate[tag];
      If[Length[SelectedCells[]] > 1,
        Message[tagged::shdw, tag],
        If[OptionValue[tagged, "final"],
          FrontEndExecute[FrontEndToken["OpenCloseGroup"]];
  				SelectionMove[NextCell[CellStyle -> "Input"], All, Cell];
        ]
      ];
		)
  ];
];
tagged[expr_, func_, opts:OptionsPattern[]] := With[{tag = ToString[HoldForm[expr]]},
  If[Not[$Notebooks], TeXPrint[func[expr], tag],
	  (
    CellPrint[ExpressionCell[
      expr // func //
        If[OptionValue[tagged, "colorize"], colorize[_HoldForm], Identity] //
        OptionValue[tagged, "form"],
      "Output", CellTags -> tag, ShowCellTags -> True, opts
    ]];
    NotebookLocate[tag];
    If[Length[SelectedCells[]] > 1,
      Message[tagged::shdw, tag],
      If[OptionValue[tagged, "final"],
        FrontEndExecute[FrontEndToken["OpenCloseGroup"]];
				SelectionMove[NextCell[CellStyle -> "Input"], All, Cell];
      ]
    ];
		)
  ];
];


SetAttributes[untagged, HoldFirst]
untagged[expr_, opts:OptionsPattern[]] := untagged[expr, Identity, opts];
untagged[expr_, func_:Identity, opts:OptionsPattern[]] := With[{tag = ToString[Unique["eq"]]},
  If[Not[$Notebooks],  TeXPrint[func[expr], tag],
    (
		  CellPrint[ExpressionCell[
			  expr // func  //
          If[OptionValue[untagged, "colorize"], colorize[_HoldForm], Identity] //
				  OptionValue[untagged, "form"],
				"Output", CellTags->tag, ShowCellTags -> True
      ]];
      NotebookLocate[tag];
      If[Length[SelectedCells[]] > 1,
        Message[tagged::shdw, tag],
        If[OptionValue[untagged, "final"], (
            FrontEndExecute[FrontEndToken["OpenCloseGroup"]];
    		    SelectionMove[NextCell[CellStyle -> "Input"], All, Cell];
				  ),
					SelectionMove[EvaluationNotebook[], Previous, CellContents];
        ]
      ];
    );
  ];
];


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

getRunner[] := CellPrint[ExpressionCell[ReleaseHold[buttons[EvaluationNotebook[]]],
  "Text", CellTags->"run", ShowCellTags -> True, GeneratedCell->False
]];

getRunner[nb_NotebookObject] := CreateWindow[
  PaletteNotebook[ReleaseHold[buttons[nb]]],
  WindowTitle -> "Run_" <> Last[FileNameSplit[NotebookFileName[nb]]],
  WindowFloating -> True
];


UnderBar = HoldForm;


row[l_List] := Row[l, ",\t"];
row[expr_] := expr;

column[l_List] = Column[l, Spacings->1.5];
column[expr_] := expr;


Options[grid] = {
    Background -> {None, {{Lighter[Blend[{Black, White}], .95], Lighter[Blend[{Black, White}], .75]}}}
   	, Dividers -> {{Darker[Gray, .6], {Lighter[Gray, .5]}, Darker[Gray, .6]}, {Darker[Gray, .6], Darker[Gray, .6], {False}, Darker[Gray, .6]}}
   	, Alignment -> Right
   	, Frame -> Darker[Gray, .6]
   	, ItemStyle -> 14
   	, Spacings -> {Automatic, .8}
};
grid[l_List, title_List:{}, opts___] := grid[Map[List, l], title, opts];
grid[l:{{___} ..}, title_List:{}, OptionsPattern[]] := With[
  {
	  fulltable = Prepend[l, If[title=!={}, title, ConstantArray["", Length[First[l]]]]]
  },
	Grid[
	  fulltable
    , Background -> OptionValue[grid, Background]
    , Dividers -> OptionValue[grid, Dividers]
    , Alignment -> OptionValue[grid, Alignment]
    , Frame -> OptionValue[grid, Frame]
    , ItemStyle -> OptionValue[grid, ItemStyle]
    , Spacings -> OptionValue[grid, Spacings]
   ]
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


SetAttributes[matrixComplexityPlot, HoldFirst]
matrixComplexityPlot[m_, label_: Style["Matrix complexity", Italic]] := (
  Labeled[With[{n = Length[m]},
     With[{idxs = Range[n], ticks = Range[0, n + 1]},
      MatrixPlot[
       MapAt[If[0 === #, 0, Log10[1 + LeafCount[#]]] &, m, {All, All}],
       Frame -> True,
       FrameTicks -> {idxs, None, None, idxs},
       PlotLegends -> BarLegend[Automatic,
         LegendLabel -> DisplayForm[RowBox[{SubscriptBox["log", 10], "(# terms)"}]]
       ],
       Epilog -> {
         Thin, Gray, Dotted,
         InfiniteLine[{#, 0}, {0, 1}] & /@ ticks,
         InfiniteLine[{0, #}, {1, 0}] & /@ ticks
         }
       ]
      ]
     ], label, Top] // Framed[#, FrameStyle -> Thin, FrameMargins -> 20] &
) /; MatrixQ[m] && Equal @@ Dimensions[m];


matrixComplexityPlot[m_] := matrixComplexityPlot[m, HoldForm[m]];


matrixPlot[expr_] := (expr //
  MapAt[If[# === 0, ".", "#"]&, #, {All, All}]& //
  Map[StringJoin] //
  Riffle[#, "\n"]& //
  Prepend["\n"] //
  StringJoin //
  print
);


End[];


(* Print[$Context]; *)


EndPackage[];
