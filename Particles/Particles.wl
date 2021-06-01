(* ::Package:: *)

(* ::Text:: *)
(*Helpers for for ParticleData*)


BeginPackage["RG`Particles`", {"RG`BaseUtils`"}]


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


On[General::stop];


End[]


EndPackage[]
