datum = Date[];
protoIn = StringJoin["in_",
  ToString[datum[[3]]], "_",
  ToString[datum[[2]]], ".m"
];
protoOut = StringJoin["out_",
  ToString[datum[[3]]], "_",
  ToString[datum[[2]]], ".m"
];
protoInStream = OpenAppend[protoIn];
protoOutStream = OpenAppend[protoOut];
Print["[....]: Protocol file ", "'", protoIn, "'", " opened"];
Print["[....]: Protocol file ", "'", protoOut, "'", " opened"];
WriteString[protoInStream, "\n(*"<>DateString[]<>"*)\n"];
WriteString[protoInStream, "In[1]:= "];
WriteString[protoOutStream, "\n(*"<>DateString[]<>"*)\n"];
$Pre=(WriteString[protoInStream, "\nIn["<>ToString[$Line + 1]<>"]:= "];#)&
AppendTo[$Echo, protoInStream];
AppendTo[$Output, protoOutStream];
