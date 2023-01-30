open Card
open Init_freecell
open Init_seahaven
open Init_midnight_oil
open Init_baker
open Gestion_partie




val recherche_solution  : etat -> (etat-> coup list) -> (etat -> coup list -> etat list) -> out_channel -> unit