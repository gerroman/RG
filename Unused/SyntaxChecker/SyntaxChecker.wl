BeginPackage["RG`SyntaxChecker`"]


syntaxError::usage = "syntaxError[text, n] \[LongDash] search for simple syntax errors in the Wolfram Language 'text'"


syntaxChecker::usage = "syntaxChecker[fname] \[LongDash] check syntax errors in the file 'fname', print fragment where error may occur"


Begin["`Private`"]


syntaxError[text_String, n_Integer:1] := Module[
	{result, pos, splits, pre, prepos, postpos},
	pos = SyntaxLength[text];
	splits = StringPosition[text, "\n"];
	pre = LengthWhile[splits, #[[1]] < pos&];
	prepos = splits[[If[pre - n > 0, pre - n, 1]]][[2]] + 1;
	postpos = If[pre + n < Length[splits], splits[[pre + n + 1]][[2]] - 1, StringLength[text]];
	Return[{pre + 1, StringTake[text, {prepos, postpos}]}];
];


syntaxChecker[fname_String] := Module[
	{result, text, line, fragment},
	If[Not@FileExistsQ[fname],
		WriteString["stderr", StringForm["[error]: file '``' not found\n", fname]];
		Return[False];
	];

	With[{f = OpenRead[fname]},
		text = (StringJoin @@ Riffle[ReadList[f, String], "\n"]);
		Close[f];
	];

	result = SyntaxQ @ text;

	WriteString["stderr",
		StringForm["\033[1;34m[syntax]\033[0m: `` ... ``\n",
			StringPadRight["\033[1;36m"<>FileNameTake[fname]<>"\033[0m ", 69, "."],
			If[result, "\033[1;32m[OK]\033[0m", "\033[1;31m[ERROR]\033[0m"]
		]
	];

	If[Not@result,
		{line, fragment} = syntaxError[text];
		WriteString["stderr", StringForm["``:``: syntax error in (or above) the lines: \n", fname, line]];
		Write["stderr", " ... "];
		Write["stderr", fragment];
		Write["stderr", " ... "];
	];

	result
];


End[]


EndPackage[]
