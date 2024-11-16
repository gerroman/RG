(* ::Package:: *)


Install[LinkConnect["6464"]]

Print[AddTwo32[10, 2^31-11]]
Print[AddTwo32w[10, 2^31-11]]
Print[AddTwo64[10, 2^31-11]]
Print[AddTwo64w[10, 2^31-11]]

LinkClose/@Links["6464"]
