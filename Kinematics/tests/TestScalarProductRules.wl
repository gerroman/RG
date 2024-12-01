Needs["RG`Kinematics`ScalarProductRules`"]


sp=ScalarProduct;


GetScalarProductRules[{p1}, {sp[p1] == 0}]


GetScalarProductRules[{p1}, {}]


GetScalarProductRules[{p1, -p2}, {sp[p1] == me2}]


GetScalarProductRules[{p1, p2, p3, p4}, {sp[p1 + p2] == s, sp[p1 - p3] == t}]
