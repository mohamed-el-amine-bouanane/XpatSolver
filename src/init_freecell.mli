open Card
open Gestion_partie








val add_element : card ->int-> card list FArray.t->card list FArray.t

val init_colonne : int list -> card list FArray.t-> int ->int -> card list FArray.t

val init_freecell : int list ->etat


val source_valide_fr : int -> etat -> bool

val destination_valid_fr : int -> etat -> bool

val cond_carte : card -> card -> bool

val coup_carte_legal_fr : int -> int-> bool

val  traite_carte_vers_carte_fr : int -> int -> etat -> etat

val traite_coup_vers_colonne_vide_fr : int -> etat -> etat

val traite_coup_vers_registre_tmp_fr : int -> etat -> etat

val coups_valide_freecell : etat-> coup list -> coup list

val gen_coup_fr : etat -> coup list

val effectuer_coups_et_normalisation_fr : etat -> coup list -> etat list 



