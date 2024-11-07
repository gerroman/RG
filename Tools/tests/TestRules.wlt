Get["RG/Tools/Rules.wl"]


VerificationTest[
  (x //. rule`hold[x])
  ,
  Hold[x]
]


VerificationTest[
  Hold[x] + Hold[y] //. rule`release[x]
  ,
  Hold[y] + x
]


VerificationTest[
  (x^2)^(p) //. rule`powerExpand[x]
  ,
  x^(2 p)
]


VerificationTest[
  Sin[Abs[(x^2)^(1/2)]] //. rule`powerExpand[x]
  ,
  Sin[x]
]


VerificationTest[
  1 + x a y - x b y //. rule`factor[x y]
  ,
  1 + x y (a - b)
]


VerificationTest[
  1 + x a //. rule`factor[x]
  ,
  1 + a x
]


VerificationTest[
  1 + a x //. rule`pull[x]
  ,
  x (1/x + a)
]
