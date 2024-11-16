(* ::Package:: *)

Needs["RG`Integrate`"]


integrate[Sin[x]^2,{x, 0, Pi}] // force[integrate]


integrate[Sin[x],{x,0,Pi}] // indetermineIntegrate // force[integrate]


eq`int[1] = eq[integrate[1/(1-x^2),x], {}, {
  changeIntegrateVars[{x->Sin[y]}, {y->ArcSin[x]}], 
  Simplify,
  groupIt[HoldForm[1/Cos[y]], ReleaseHold]
}]


eq`int[1] //
  determineIntegrate[{x, 0, 1/2}] //
  determineIntegrate[{y, 0, Pi/6}] //
  ReleaseHold // 
  force[integrate]//
  FullSimplify


substitute[(1-y)y==\[Sigma]^2/4, y, \[Sigma]]



