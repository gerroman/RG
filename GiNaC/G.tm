:Begin:
:Function:       EvalG
:Pattern:        EvalG[zs_List, y_Real]
:Arguments:      {zs, y}
:ArgumentTypes:  {RealList, Real}
:ReturnType:     Manual
:End:


:Begin:
:Function:       EvalG
:Pattern:        EvalG[zsRe_List, zsIm_List, y_Real]
:Arguments:      {zsRe, zsIm, y}
:ArgumentTypes:  {RealList, RealList, Real}
:ReturnType:     Manual
:End:

:Evaluate: EvalG::usage = "EvalG[{z1, ..., zn}, y] -- for Real {z1, ..., zn}, and Real y, EvalG[{Re[z1], ..., Re[zn]}, {Im[z1], .., Im[zn]}, y] -- for Complex {z1, ..., zn} and Real y"
