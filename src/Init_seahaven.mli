open Card
open Gestion_partie


  


val affiche_depot : etat -> unit

val affiche_registre : etat ->unit

val affiche_pile : card list-> unit

val affiche_colonnes :  etat -> unit

val add_element : card ->int-> card list FArray.t->card list FArray.t

val add_element_registre :card->card list->card list

val init_registres_seaheaven: int list->card list->int->card list

val init_colonnes_seaheaven : int list -> card list FArray.t-> int -> int ->card list FArray.t

val init_seahaven : int list ->etat

val source_valide_st : int -> etat -> bool

val destination_valid_st : int -> etat -> bool

val cond_carte : card -> card -> bool

val coup_carte_legal_st : int -> int-> bool

val  traite_carte_vers_carte : int -> int -> etat -> etat

val traite_coup_vers_colonne_vide : int -> etat -> etat

val traite_coup_vers_registre_tmp : int -> etat -> etat

val coups_valide_seahaven : etat-> coup list -> coup list

val gen_coup : etat -> coup list

val effectuer_coups_et_normalisation_st : etat -> coup list -> etat list 