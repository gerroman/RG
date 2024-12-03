(* ::Package:: *)

Needs["RG`Notation`"]


setIndexed[x]
{x[i], x[i, j], x[{i, j}, x[k]]} // TraditionalForm
setSubscript[y]
{y[x[i]]}//TraditionalForm
setSuperscript[z]
{z[y[x[i]]]}//TraditionalForm


z[y[x[i]]]//TeXForm
