If[ValueQ[protoIn] && ValueQ[protoOut], (
    Clear[$Pre,$Post];
    {$Echo, $Output} = {DeleteCases[$Echo, protoInStream], DeleteCases[$Output, protoOutStream]};
    WriteString[protoInStream, "\n"];
    Scan[Close, {protoInStream, protoOutStream}];
    Print["[end.m]: note that ..."];
    Print["[....]: Protocol file ", "'", protoIn, "'", " updated"];
    Print["[....]: Protocol file ", "'", protoOut, "'", " updated"];
  ),
  Print["[end.m]: nothing to do ..."];
];
