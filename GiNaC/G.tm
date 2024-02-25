:Begin:
:Function:       G
:Pattern:        G[z_Real /; (z >= 1 || z < 0), y_Real /; (0 <= y && y < 1)]
:Arguments:      {z, y}
:ArgumentTypes:  {Real, Real}
:ReturnType:     Real
:End:

:Evaluate: G::usage = "G[z, y] evaluates G[{z}, y] for real values of z (z >= 1 || z < 0) and y ((0 <= y && y < 1))"
