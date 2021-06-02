(* ::Package:: *)

(* ::Text:: *)
(*Helpers for for ParticleData*)


BeginPackage["RG`Particles`", {"RG`BaseUtils`", "RG`Notation`", "RG`Presentation`"}]


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

describeProcess::usage = "
  describeProcess[process] \[LongDash] describe process as transition of  particle properties 
"
ShowMissing::usage = "
  ShowMissing \[LongDash] True/False value option for describeProcess
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


anti[particle_] := ParticleData[particle, "Antiparticle"];


symbolizeParticles[expr_] := ReplaceAll[expr, Entity["Particle", p_] :> ParticleData[p, "Symbol"]];
symbolizeParticles[expr_, All] := ReplaceAll[expr, {
  Entity["Particle", p_] :> ParticleData[p, "Symbol"]
  , p_String :> ParticleData[p, "Symbol"]
}];



SetAttributes[getProperty, Listable];
getProperty[property_][Entity["Particle", x_]] := ParticleData[x, property];
getProperty[property_][name_String] := ParticleData[name, property];



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


On[General::stop];


End[]


EndPackage[]
