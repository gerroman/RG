BeginPackage["RG`Series`"]

leadingTermOrder::usage = "leadingTermOrder[series] \[LongDash] get order of the leading term in 'series' expansion"

seriesOrder::usage = "seriesOrder[series] \[LongDash] get order the last term in 'series' expansion"

leadingTerm::usage = "leadingTerm[series] \[LongDash] get the leading term (as a series with a single term) in 'series'"

subLeadingTerms::usage = "subLeadingTerms[series] \[LongDash] get terms remaining after subtracting the leading term from 'series'"

leading::usage = "leading[series] \[LongDash] get leadingTerm + O[subLeadingTerms] for the 'series'
leading[series, n] \[LongDash] get the first 'n' leadingTerms + O[remainderTerms]"

seriesToRules::usage = "seriesToRules[series] \[LongDash] converts series to rules for coefficents"


Begin["`Private`"]


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
subLeadingTerms[s_SeriesData] := leadingTerm[s - Normal@leadingTerm[s]];


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


SetAttributes[leading, Listable];
leading[s_SeriesData] := Module[
	{lo = Normal[leadingTerm[s]], var, pole, nmin, nmax, den},
	{var, pole, nmin, nmax, den} = getSeriesData[s - lo];
  lo + SeriesData[var, pole, {}, nmin, nmin, den]
];

leading[s_SeriesData, n_Integer /; n > 0] := Module[
  {
		expansion = Reap[
      NestWhile[
        (# - Sow[Normal@leadingTerm[#]]) &,(*func*)
        s,(*expr*)
        Normal@leadingTerm[#] =!= 0 &, (*test*)
        1,(*most resent results to test*)
        n
	  	]
		],
		var, pole, nmin, nmax, den
  },
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


End[]


EndPackage[]
