Needs["RG`Scripts`"]

Print["[info]: testing ..."]
log["testing log[] ... "]
error["testing error[] ..."]
warning["testing warning[] ..."]
echo["testing echo[] ..."]

echo[argparse[]]
argparse["value", 1]
argparse["flag", False]
argparse["script", "test"]

timeStamp[]
systemStamp[]

echo[head[$InputFileName, 10]]

echo[sizeOf[Range[10]]]

info[argparse]