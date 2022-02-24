(* ::Package:: *)
(* Utils to present results of evaluation *)


BeginPackage["RG`Presentation`", {"RG`BaseUtils`"}];


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
";


Begin["`Private`"];


Options[tagged] = {"form" -> TraditionalForm, "colorize" -> True, "final" -> False};
Options[untagged] = {"form" -> TraditionalForm, "colorize" -> True, "final" -> False};


SetAttributes[tagged, HoldFirst];
tagged::shdw = "Warning: `` appeares more than once, so it can shadow the previous result";
tagged[expr:Set[lhs_, _], args___] := (
  expr;
  tagged[lhs, args];
);
tagged[expr_, opts:OptionsPattern[]] := tagged[expr, Identity, opts];
tagged[expr_, func_:Identity, opts:OptionsPattern[]] := (
  If[Not[$Notebooks], Return[expr]];
  With[{tag = ToString[HoldForm[expr]]},
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
        FrontEndExecute[FrontEndToken["OpenCloseGroup"]]
      ]
    ];
		SelectionMove[NextCell[CellStyle -> "Input"], All, Cell];
  ];
);


SetAttributes[untagged, HoldFirst]
untagged[expr_, opts:OptionsPattern[]] := untagged[expr, Identity, opts];
untagged[expr_, func_:Identity, opts:OptionsPattern[]] := (
  If[Not[$Notebooks]
    , expr
    , With[{tag = ToString[Unique["eq"]]}, CellPrint[ExpressionCell[
      expr // func  //
        If[OptionValue[untagged, "colorize"], colorize[_HoldForm], Identity] //
				OptionValue[untagged, "form"],
	"Output", CellTags->tag, ShowCellTags -> True
    ]];
    NotebookLocate[tag];
    If[Length[SelectedCells[]] > 1,
      Message[tagged::shdw, tag],
      If[OptionValue[untagged, "final"],
        FrontEndExecute[FrontEndToken["OpenCloseGroup"]]
      ]
    ];
		SelectionMove[NextCell[CellStyle -> "Input"], All, Cell];
    ];
  ];
);


colorize[xs_List] := With[{n = Length[xs]},
  With[{styles = If[n > 0, Array[i \[Function] ColorData["DarkRainbow"][i / Max[1, n - 1]], n, 0], {}]},
    With[{rules = Thread[Inner[(#1 -> Style[#1, #2])&, xs, styles, List]]},
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
   Button["Help",
     FrontEndExecute[FrontEndTokenExecute[nb, "SelectionHelpDialog"]]
   ]},
   {
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
   Button["Quit", (
       If[nb =!= EvaluationNotebook[],
          SetSelectedNotebook[nb];
          NotebookClose[ButtonNotebook[]];
       ];
       Quit[];
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


row = Row[#, ",\t"] &;
column = Column[#, Spacings->1.5]&;


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
    str = ToString[StringForm["[`` terms in total]", Length[{xs}]]],
    func = If[OptionValue["HoldForm"], HoldForm, Identity],
    xn = {xs}[[;; Min[OptionValue["n"], Length[{xs}]]]]
  },
  With[{args=func /@ xn // Append[str]}, head@@args]
];


End[];


Echo[$Context];


EndPackage[];
