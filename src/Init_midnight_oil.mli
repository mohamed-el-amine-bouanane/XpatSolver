open Card
open Gestion_partie 




val affiche_depot : etat -> unit

val affiche_registre : etat ->unit

val affiche_pile : card list-> unit

val affiche_colonnes :  etat -> unit

val add_element : card ->int-> card list FArray.t->card list FArray.t

val add_element_registre : card -> card list -> card list

val init_colonnes_midnight_oil : int list -> card list FArray.t-> int->card list FArray.t

val init_midnight_oil : int list ->etat

val source_valide_mo : int -> etat -> bool

val destination_valid_mo : int -> etat -> bool

val cond_carte : card -> card -> bool

val coup_carte_legal_mo : int -> int-> bool

val traite_coup_vers_colonne_vide : int -> etat -> etat

val traite_coup_vers_registre_tmp : int -> etat -> etat

val  traite_carte_vers_carte : int -> int -> etat -> etat

val gen_coup : etat -> coup list

val effectuer_coups_et_normalisation_mo : etat -> coup list -> etat list