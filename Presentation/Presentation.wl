(* ::Package:: *)
(* Utils to present results of evaluation *)


BeginPackage["RG`Presentation`"]


tagged::usage = "
  tagged[eq`tag = ...] make definition for eq`tag and produce output cell with the tag \"eq`tag\"
  tagged[expr] evaluate expr and produce output cell with the tag \"expr\"
"

tagged`final = False;


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
    CellPrint[ExpressionCell[expr // func // form, "Output", CellTags -> tag, ShowCellTags -> True, opts]];
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


End[]



EndPackage[]