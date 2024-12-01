Get["RG`Kinematics`PhysicalRegion`"]

sp=ScalarProduct;

rule`sp = GetScalarProductRules[{p1,p2,-k1,-k2}, {
  sp[p1]==0, sp[p2]==0, sp[k1]==0, sp[k2]==0,
  sp[p1-k2] == t, sp[p1-k1] == u
}]//Simplify

sp[p1+p2] + t + u//EvaluateScalarProducts[#, rule`sp]&


GetPhysicalRegion[{{p1, p2}, {k1, k2}}, {
  sp[p1]==0, sp[p2]==0, sp[k1]==0, sp[k2]==0,
  sp[p1+p2]==s, sp[p1-k1] == -s Sin[\[Theta]/2]^2
}] // PowerExpand // Union


GetPhysicalRegion[{{p1}, {k1, k2}}, {
  sp[p1]==M^2, sp[k1]==mmu^2, sp[k2]==mmu^2
}] // PowerExpand // Union



GetPhysicalRegion[{{p1, p2}, {k1, k2}}, {
  sp[p1]==mmu^2, sp[p2]==mmu^2, sp[k1]==me^2, sp[k2]==me^2,
  sp[p1 + p2] == s, sp[p1 - k1] == t
}] // Union
