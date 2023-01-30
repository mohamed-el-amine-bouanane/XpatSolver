open Card
open Gestion_partie

  




val affiche_depot : etat -> unit

val affiche_registre : etat ->unit

val affiche_pile : card list-> unit

val affiche_colonnes :  etat -> unit

val add_element : card ->int-> card list FArray.t->card list FArray.t

val add_element_registre : card ->  card list -> card list

val est_roi : card ->bool

val colonne_correcte : card list -> bool

val descendre_roi_colonne : card list -> card list

val descendre_roi_tableau_colonne : card list FArray.t ->int -> card list FArray.t

val init_colonnes_baker : int list -> card list FArray.t-> int->card list FArray.t

val init_baker : int list ->etat

val source_valide_bd : int -> etat -> bool

val destination_valid_bd : int -> etat -> bool

val cond_carte : card -> card -> bool

val coup_carte_legal_bd : int -> int-> bool

val  traite_carte_vers_carte : int -> int -> etat -> etat

val traite_coup_vers_colonne_vide : int -> etat -> etat

val traite_coup_vers_registre_tmp : int -> etat -> etat

val gen_coup_bd : etat -> coup list

val effectuer_coups_et_normalisation_bd : etat -> coup list -> etat list