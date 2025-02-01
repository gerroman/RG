(* ::Package:: *)

BeginPackage["RG`ExtractEquations`"];


GetEquations::usage="GetEquations[filename, env] \[LongDash] extract \\begin{env}...\\end{env} fragments \
from TeX file. Return a list {{l, fragment}..} meaning that fragment appears at line l of the \
file.\n\
GetEquations[filename] \[LongDash] extract equations within environments {equation, align, multline, \
gather, eqnarray}"
ParseEquation::usage="ParseEquation[fragment_String, opts] \[LongDash] convert TeX fragment to \
Mathematica expression. \
Setting the option \"rules\" (default = {}) define a list of string substitution rules applied \
just before calling ToExpression[]. \
Setting the option \"verbose\" \[Rule] True (default = False) allow to see intermidiate results."
PrintEquation::usage="PrintEquation[fragment_String] \[LongDash] perform a sequence of actions: \
(1) save fragment to temporary TeX file, \
(2) create pdf using pdflatex, \
(3) convert pdf to png using pdftoppm, \
(4) import png image. Options: \
\"preambula\" define TeX file preambula (default = \"\\documentclass[varwidth=8in,crop]{standalone}...\"); \
\"rules\" define a list of string substitution rules (default = {}) applied \
just before saving TeX file; \
\"basename\" defines auxiliary file name (excluding extension .tex, .pdf, .png)); \
\"verbose\" \[Rule] True (default = False) allow to see intermidiate results. \
"


Begin["`Private`"]


GetEquations::notfound="[error]: file '``' not found";
GetEquations::notmatch="[error]: unmatched '``' environment";


Options[GetEquations]={"verbose"->False}


GetEquations[fname_, env_String, opts:OptionsPattern[]] := Module[{
  text, posIn, posOut, verbose=OptionValue["verbose"]
},
If[verbose, PrintTemporary["reading ", fname]];
If[Not@FileExistsQ[fname],(
  Message[GetEquations::notfound,fname];
  Return[$Failed];
)];
With[{f=OpenRead[fname]},
  text=ReadList[f, String];(*read file content as list of strings*)
  Close[f];
];
(*remove trailing spaces*)
text=Map[StringTrim, text];
(* skip TeX comments *)
text=DeleteCases[text, s_String/;StringMatchQ[s,"%"~~__]];
If[verbose, PrintTemporary["search for ", env]];
(* find begin positions *)
posIn=Flatten[Position[text, s_String/;StringStartsQ[s,"\\begin{"<>env<>"}"]]];
(* find end positions *)
posOut=Flatten[Position[text, s_String/;StringStartsQ[s,"\\end{"<>env<>"}"]]];
If[Length[posIn]!=Length[posOut], (
  Message[GetEquations::notmatch, env];
  Return[$Failed];
)];
{#[[1]], StringJoin[Riffle[text[[#[[1]];;#[[2]]]], "\n"]]}& /@ Transpose[{posIn,posOut}]
];


GetEquations[fname_, opts:OptionsPattern[]] := Sort[Join@@(GetEquations[fname, #, opts]& /@ {
  "equation",
  "equation*",
  "align",
  "align*",
  "gather",
  "gather*",
  "multline",
  "multline*",
  "eqnarray",
  "eqnarray*"
})];


Options[ParseEquation]={"verbose"->False, "rules"->{}}


ParseEquation[s_String, opts:OptionsPattern[]] := Module[{
  tmp = s,
  phase=0,
  rules=OptionValue["rules"],
  verbose=OptionValue["verbose"]
},
If[verbose, Print[phase++,": (initial): ",tmp]];
tmp=StringTrim[s, RegularExpression["\\s*\\\\(begin|end){[^}]+}\\s*"]];
If[verbose, Print[phase++,": (trim environment): ",tmp]];
tmp = StringReplace[tmp, {
  "\n"->" ",
  "\\qquad"->" ",
  "\\quad"->" ",
  "\\nonumber"->" ",
  "\\\\" ->" ",
  "&"->" ",
  "\\left."->" ",
  "\\right."->" "
}];
If[verbose, PrintTemporary[phase++,": (remove TeX delimiters): ",tmp]];
tmp = StringReplace[tmp, RegularExpression["\\\\label{[^}]+}"]->" "];
If[verbose, PrintTemporary[phase++,": (remove labels): ",tmp]];
tmp = StringTrim[tmp];
If[verbose, PrintTemporary[phase++,": (remove trailing spaces): ",tmp]];
tmp = StringTrim[tmp, "\\,,"|"\\,."];
If[verbose, PrintTemporary[phase++,": (remove trailing punctuation): ",tmp]];
tmp = StringSplit[tmp, "\\,,"|"\\,."];
If[verbose, PrintTemporary[phase++,": (split by inner punctuation): ",tmp]];
tmp = Map[StringTrim, tmp];
If[verbose, PrintTemporary[phase++,": (remove trailing spaces): ",tmp]];
tmp = Fold[StringReplace[#1, #2]&, tmp, rules];
If[verbose, PrintTemporary[phase++,": (apply final rules): ",tmp]];
tmp = MapIndexed[(
  PrintTemporary[ToString@StringForm["``: ``", #2[[1]], #1]];
  ToExpression[#1,TeXForm,HoldForm]
)&, tmp];
Return[If[Length[tmp]==1, tmp[[1]], tmp]]
]


Options[PrintEquation]={
"preambula"->"\
\\documentclass[varwidth=8in,crop]{standalone}\n\
\\usepackage{amsmath}\n\
\\usepackage{amssymb}\
",
"basename" :> FileNameJoin[{$TemporaryDirectory,"tmp"}],
"verbose" -> False,
"rules" -> {}
}


PrintEquation[s_String, opts:OptionsPattern[]] := Module[{
  preambula = OptionValue["preambula"],
  phase = 0,
  tmp = s,
  content,
  tex = OptionValue["basename"]<>".tex",
  pdf = OptionValue["basename"]<>".pdf",
  png = OptionValue["basename"]<>".png",
  template = StringTemplate["``\n\\begin{document}\n``\n\\end{document}"],
  result,
  verbose = OptionValue["verbose"],
  rules = OptionValue["rules"]
},
If[verbose, PrintTemporary[phase++,": (initial equation):\n",tmp]];
tmp = StringReplace[s, {
  "{equation}"->"{equation*}",
  "{align}"->"{align*}",
  "{gather}"->"{gather*}",
  "{multline}"->"{multline*}"
}];
If[verbose, PrintTemporary[phase++, ": (unnumbered equation):\n", tmp]];
tmp = StringReplace[tmp, rules];
If[verbose, PrintTemporary[phase++, ": (applying rules):\n", tmp]];
content = template[preambula, tmp];
If[verbose, PrintTemporary[phase++, ": (adding preambula):\n", content]];
If[verbose, PrintTemporary[phase++, ": (saving TeX file): ", tex]];
With[{fstream=OpenWrite[tex]}, (
  WriteString[fstream, content];
  Close[fstream];
)];
If[verbose, PrintTemporary[phase++, ": running pdflatex ... "]];
result = RunProcess[{"pdflatex",
"-output-directory", DirectoryName[tex],
"-output-format", "pdf",
"-interaction", "nonstopmode",
tex}];
If[result["ExitCode"] != 0, (
  Print[content];
  Print[result["StandardOutput"]];
  Return[$Failed];
)];
If[verbose, PrintTemporary[phase++, ": running pdftoppm ... "]];
result = RunProcess[{"pdftoppm", "-r", "150", "-png", pdf}];
If[result["ExitCode"] != 0, (
  Print[content];
  Print[result["StandardError"]];
  Return[$Failed];
)];
If[verbose, PrintTemporary[phase++, ": (saving PNG file): ", png]];
With[{fstream=OpenWrite[png,BinaryFormat->True]}, (
  WriteString[fstream, result["StandardOutput"]];
  Close[fstream];
)];
If[verbose, PrintTemporary[phase++, ": (importing PNG file ", png]];
Import[png]
]


End[]


EndPackage[]
