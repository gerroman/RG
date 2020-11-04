(* ::Package:: *)
(* Utils to present results of evaluation *)


BeginPackage["RG`Presentation`"]


tagged::usage = "
  tagged[eq`tag = ...] make definition for eq`tag and produce output cell with the tag \"eq`tag\"
  tagged[expr] evaluate expr and produce output cell with the tag \"expr\"
"

tagged`final = False;


colorize::usage = "
  colorize[pattern] colorize matches for the pattern
  colorize[{x1, ...}] colorize specific expressions x1, ...
";


Begin["`Private`"]


tagged::shdw = "Warning: `` appeares more than once so can shadow previous result";
SetAttributes[tagged, HoldAll];
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


End[]


EndPackage[]