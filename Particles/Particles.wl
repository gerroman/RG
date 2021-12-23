(* ::Package:: *)

(* ::Text:: *)
(*Helpers for for ParticleData*)


BeginPackage["RG`Particles`", {"RG`BaseUtils`",
							  "RG`CommonNotation`", 
							  "RG`Notation`",
							  "RG`Presentation`"}]


classes::usage = "
  classes \[LongDash] list of particle classes
"


particles::usage = "
  particles \[LongDash] list of particles
"


properties::usage = "
  properties[\"All\"] \[LongDash] all known properties
  properties[\"Basic\"] \[LongDash] basic properties
  properties[\"QuantumNumbers\"] \[LongDash] quantum numbers
  properties[\"LongLived\"] \[LongDash] properties of long-lived particles
  properties[\"Related\"] \[LongDash] related particles
  properties[\"Decay\"] \[LongDash] decay
  properties[\"Names\"] \[LongDash] decay
"


anti::usage = "
  anti[particle] \[LongDash] return anti-particle
"


symbolizeParticles::usage = "
  symbolizeParticles[expr] \[LongDash] replace Entity[\"Particle\", p] to ParticleData[p, Symbol]
  symbolizeParticles[expr, All] \[LongDash] also replace all strings
"


getProperty::usage = "
  getProperty[property][expr] \[LongDash] replace particle to its property value
"


getDecays::usage = "
  getDecays[particle] \[LongDash] return list of particle decays
"

getDecaysTable::usage = "
  getDecaysTable[particle, nMax] \[LongDash] format particle first nMax decays as grid 
"

getFrequentDecays::usage = "
  getFrequentDecays[particle, branching] return list of frequent decays to long-lived particles
"

describeProcess::usage = "
  describeProcess[process] \[LongDash] describe process as transition of  particle properties 
"

ShowMissing::usage = "
  ShowMissing \[LongDash] True/False value option for describeProcess
"

describeParticle::usage = "
  describeParticle[particle] - list particle properties 
"

getJPC::usage = "
  getJPC[particle] - get list {J, P, C} of particle's Spin, Parity and C-Parity
"
formatJPC::usage = "
  formatJPC[particle] - format  particle's Spin, Parity and C-Parity as J^{PC}
"
getIG::usage = "
  getIG[particle] - get list {I, G} of particle's Isospin, G-Parity
"
formatIG::usage = "
  formatIG[particle] - format particle's Isospin, G-Parity as I^{G}
"


Begin["`Private`"]


Off[General::stop];


load["classes.mx", classes, 
  classes = ParticleData["Classes"]
]


load["particles.mx", particles, 
  particles = ParticleData[] // SortBy[ParticleData[#, "Mass"] &]
]


load["properties.mx", properties, (
  properties = <||>;

  properties["All"] = ParticleData["Properties"];
  
  properties["Basic"] = {
     "Symbol", "Charge", "Mass", "Width", "Lifetime", "HalfLife", 
     "QuarkContent", "Memberships"
  };
  properties["QuantumNumbers"] = {
     "BaryonNumber", "Bottomness", "Charm", "CParity", "GParity", 
     "Hypercharge", "Isospin", "IsospinProjection", "LeptonNumber", 
     "Parity", "Spin", "Strangeness", "Topness"
  };
  properties["LongLived"] = {
     "GFactor", "MeanSquareChargeRadius"
  };
  properties["Related"] = {
     "Antiparticle", "ChargeStates", "Excitations", "IsospinMultiplet"
  };
  properties["Decay"] = {
     "DecayModes", "DecayType", "FullDecayModes", "UnobservedDecayModes"
  };
  properties["Names"] = {
     "FullSymbol", "GenericFullSymbol", "GenericSymbol", "PDGNumber", 
     "StandardName"
  };
)]


On[General::stop];


anti[particle_] := ParticleData[particle, "Antiparticle"];


symbolizeParticles[expr_] := ReplaceAll[expr, Entity["Particle", p_] :> ParticleData[p, "Symbol"]];
symbolizeParticles[expr_, All] := ReplaceAll[expr, {
  Entity["Particle", p_] :> ParticleData[p, "Symbol"]
  , p_String :> ParticleData[p, "Symbol"]
}];



SetAttributes[getProperty, Listable];
getProperty[property_][Entity["Particle", x_]] := ParticleData[x, property];
getProperty[property_][name_String] := ParticleData[name, property];
getProperty[property_][l_List] := Map[getProperty[property], l];
getProperty[property_][r_Rule] := Map[getProperty[property], r];



Options[describeProcess] = {ShowMissing -> True};
With[{props = {
      "Symbol"
      , "Mass"
      , "Spin"
      , "Parity"
      , "Charge", "LeptonNumber", "BaryonNumber"
      , "CParity", "GParity", "Hypercharge"
      , "QuarkContent", "Isospin"
      , "IsospinProjection", "Strangeness", "Charm", "Bottomness", "Topness"
    }
  },
  describeProcess[process_, OptionsPattern[]] := (
    {#, getProperty[#][process]}& /@ props 
  ) // ReplaceAll[{
      Missing["NotApplicable"] -> "?!", 
      Missing["NotAvailable"] -> "?"
    }] // If[OptionValue[ShowMissing]
      , Identity
      , Select[#, (FreeQ[#, "?!", Infinity] && FreeQ[#, "?", Infinity] &)]&
    ] // grid[#, {"property", "values"}, Alignment -> Left] &;
];



getDecays[particle_String] := With[{
    decays = ParticleData[particle, "DecayModes"]
  },
  If[Head[decays] === List,
    (
       Thread[particle -> decays] //
         ReplaceAll[(a_ -> {b_, c_}) :> {Rule[a, b], c}] // 
         SortBy[Last] // Reverse
    ),
    {particle -> None}
  ]
];
getDecays[p:Entity["Particle", _]] := getDecays[p // getProperty["StandardName"]];


getDecaysTable[particle_, nMax_: 5] := With[{
    decays = getDecays[particle]
  },
  Take[decays, Min[nMax, Length[decays]]] // 
    symbolizeParticles[#, All] & // 
    grid[#, {"decay", "\[ScriptCapitalB]"}, Alignment -> Left] &
];


describeParticle[particle_] := {#, getProperty[#][particle]}& /@ {
  "Memberships", "Mass", "Width", "Charge",
  "Spin", "Parity", "CParity", "GParity",
  "Isospin", "IsospinProjection", 
  "Charm", "Strangeness", "Bottomness", "Topness",
  "DecayType", "Lifetime", "QuarkContent"
} // grid[#, {"property", "values"}, Alignment -> Left] &;


getPM[p_] := If[Head[p] === Missing, "\[EmptySquare]", plus\[LetterSpace]minus[[(3 - p) / 2]]];

getJPC[particle_] := Through@Thread[getProperty[{"Spin", "Parity", "CParity"}]][particle];
formatJPC[particle_] := Module[{j, p, c}, 
  {j, p, c} = getJPC[particle];
  p = getPM[p];
  c = getPM[c];
  Superscript[j, Row[{p, c}, ""]]
];

getIG[particle_] := Through@Thread[getProperty[{"Isospin", "GParity"}]][particle];
formatIG[particle_] := Module[{i, g}, 
  {i, g} = getIG[particle];
  g = getPM[g];
  Superscript[i, g]
];


getFrequentDecays[part_, branching_:1.*^-8] := (
  getDecays[part] //
  Select[And@@(Or[
    MemberQ[ParticleData["LongLived"], ParticleData[#]],
    MemberQ[ParticleData["Neutrino"], ParticleData[#]]
  ]& /@ Last@First[#]) &] // 
  Select[Last[#] > branching&] //
  MapAt[List, #, {{All, 1, 1}}]& //
  MapAt[ParticleData, #, {{All, 1, All, All}}]&
);


End[]


Echo[$Context];


EndPackage[]
