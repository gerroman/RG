BeginPackage["RG`Series`"]


leadingTermOrder::usage = "leadingTermOrder[series] \[LongDash] get order of the leading term in 'series' expansion"

seriesOrder::usage = "seriesOrder[series] \[LongDash] get order the last term in 'series' expansion"

leadingTerm::usage = "leadingTerm[series] \[LongDash] get the leading term (as a series with a single term) in 'series'"

subLeadingTerms::usage = "subLeadingTerms[series] \[LongDash] get terms remaining after subtracting the leading term from 'series'"

leading::usage = "leading[series] \[LongDash] get leadingTerm + O[subLeadingTerms] for the 'series'
leading[series, n] \[LongDash] get the first 'n' leadingTerms + O[remainderTerms]"

seriesToRules::usage = "seriesToRules[series] \[LongDash] converts series to rules for coefficents"

leadingSeries::usage = "leadingSeries[expr, {var, pole, order}] \[LongDash] shortcut for leading[Series[expr, {var, pole, order}]]
leadingSeries[expr, {var, pole, order}, n] \[LongDash] shortcut for leading[Series[expr, {var, pole, order}], n]"

remainder::usage = "remainder[series] \[LongDash] return the remainder of the series"

getSeriesData::usage = "getSeriesData[series] return SeriesData without coefficients"
getSeriesCoefficients::usage = "getSeriesCoefficients[series] return SeriesData without coefficients"

series::usage = "series[series] try to evaluate series faster"

Begin["`Private`"]

(* [TODO] remove Listable attributes to work with matrices *)

SetAttributes[leadingTermOrder, Listable];
leadingTermOrder[s_SeriesData] := Part[s, 4];


SetAttributes[seriesOrder, Listable];
seriesOrder[s_SeriesData] := Part[s, 5];


SetAttributes[leadingTerm, Listable];
leadingTerm[s_SeriesData] := With[{
		var = Part[s, 1],
		pole = Part[s, 2],
		nmin = Part[s, 4],
		nmax = Part[s, 5],
		den = Part[s, 6]
	},
	SeriesData @@ {
		var,
		pole,
		If[nmin < nmax, {Part[s, 3, 1]}, {}],
		nmin,
		Min[nmin + 1, nmax],
		den
	}
];


SetAttributes[subLeadingTerms, Listable];
Options[subLeadingTerms] = {Options -> "Fast"};
subLeadingTerms[s_SeriesData] := leadingTerm[s - Normal@leadingTerm[s]];
subLeadingTerms[s_SeriesData, opts:OptionsPattern[]] := (
	If[OptionValue[Options] === "Fast",
		With[{
				var = Part[s, 1],
				pole = Part[s, 2],
				nmin = Part[s, 4],
				nmax = Part[s, 5],
				den = Part[s, 6]
			},
			SeriesData @@ {
				var,
				pole,
				If[nmin + 1 < nmax, Rest@Part[s, 3], {}],
				Min[nmin + 1, nmax],
				Min[nmin + 2, nmax],
				den
			}
		],
		subLeadingTerms[s]
	]
);

SetAttributes[getSeriesData, Listable];
getSeriesData[s_SeriesData] := With[{
		var = Part[s, 1],
		pole = Part[s, 2],
		nmin = Part[s, 4],
		nmax = Part[s, 5],
		den = Part[s, 6]
	},
	{var, pole, nmin, nmax, den}
];

SetAttributes[getSeriesCoefficients, Listable];
getSeriesCoefficients[s_SeriesData] := With[{coefs = Part[s, 3]},
	Return[coefs]
];

SetAttributes[leading, Listable];
leading[s_SeriesData] := Module[
	{lo = Normal[leadingTerm[s]], var, pole, nmin, nmax, den},
	{var, pole, nmin, nmax, den} = getSeriesData[s - lo];
	lo + SeriesData[var, pole, {}, nmin, nmin, den]
];

leading[s_SeriesData, n_Integer /; n > 0] := Module[{
		expansion, var, pole, nmin, nmax, den
	},
	expansion = Reap[NestWhile[
		(# - Sow[Normal @ leadingTerm[#]]) &,(*func*)
		s,(*expr*)
		Normal @ leadingTerm[#] =!= 0 &, (*test*)
		1,(*most resent results to test*)
		n
	]];
	{var, pole, nmin, nmax, den} = getSeriesData[First[expansion]];
	Total[Last[expansion], 2] + SeriesData[var, pole, {}, nmin, nmin, den]
];

leading[s_SeriesData, 0] :=  Module[{var, pole, nmin, nmax, den},
	{var, pole, nmin, nmax, den} = getSeriesData[s];
	SeriesData[var, pole, {}, nmin, nmin, den]
];


seriesToRules[s_SeriesData] := Module[
	{var, pole, nmin, nmax, den, l, cs, coeffs},
	{var, pole, nmin, nmax, den} = getSeriesData[s];
	If[nmax == nmin, Return[{s, {}}]];
	coeffs = Part[s, 3];
	l = (var - pole)^(Range[nmin, nmax - 1] / den);
	cs = Array[C, nmax - nmin, nmin];
	Return[{
		SeriesData[var, pole, cs, nmin, nmax, den],
		Thread[cs -> coeffs]
	}]
];


leadingSeries[expr_, {var_, pole_, order_}] := leading[Series[expr, {var, pole, order}]];
leadingSeries[expr_, {var_, pole_, order_}, n_Integer] := leading[Series[expr, {var, pole, order}], n];

SetAttributes[remainder, Listable];
remainder[s_SeriesData] := Module[{var, pole, nmin, nmax, den},
	{var, pole, nmin, nmax, den} = getSeriesData[s];
	Return[SeriesData[var, pole, {}, nmax, nmax, den]];
];


(*[TODO]: process case den != 1*)
(*[TODO]: block <-> with*)
series[s_SeriesData, {var_, pole_, n0_}] := With[{data=getSeriesData[s]},
	With[{
			varSeries = data[[1]],
			poleSeries = data[[2]],
			nmin = data[[3]],
			nmax = data[[4]],
			den = data[[5]]
		},
		If[(var == varSeries && pole == poleSeries && den == 1),
			If[(n0 >= nmin),
				With[{newNMax = Min[n0 + 1, nmax]},
				  Return[SeriesData @@ {var, pole, getSeriesCoefficients[s][[1;;(newNMax - nmin)]], nmin, newNMax, 1}]
				],
				Return[SeriesData @@ {var, pole, {}, nmin, nmin, 1}]
			]
		]
	];
	Return[Hold[Series[s, {var, pole, n0}]]]
];

(*[TODO]: correct matrix series*)
(* series[expr_/;FreeQ[expr, SeriesData], {var_, pole_, nmax_}] := Module[ *)
(*   {s = Series[expr, {var, pole, n0}], nmin, nmax}, *)
(*   If[Head[s] === List, *)
(* 	  nmin = Min[leadingTermOrder[s]]; *)

(*   ] *)
(* ] *)


End[]


EndPackage[]
