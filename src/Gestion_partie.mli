open Card




type coup = int * int 

type etat =
  {
    depot : int  FArray.t;
    colonnes : card list FArray.t ;
    registres :  card list ;
    historique : coup list ;
  }

  




val affiche_depot : etat -> unit

val affiche_registre : etat ->unit

val affiche_registre_num : etat ->unit

val affiche_pile : card list-> unit

val affiche_pile_num : card list-> unit

val affiche_colonnes :  etat -> unit

val affiche_colonnes_num :  etat -> unit

val affiche_etat : etat -> unit

val affiche_coup : coup -> unit

val verif_depot_carte : card  -> int  FArray.t -> bool

val mise_en_depot_carte : card -> int  FArray.t -> int  FArray.t

val est_vide : card list  -> bool

val normaliser_colonne  : card list  -> int  FArray.t -> (int  FArray.t * card list)

val normaliser_registres : card list ->  card list ->  int  FArray.t -> (int  FArray.t * card list)

(*val copie_registres :  card list ->  card list*)

val verif_colonne_normalisable : etat ->  card list -> bool

val verif_colonnes_normalisables : card list FArray.t -> etat -> int -> bool

val verif_registres_normalisable : etat ->  card list -> bool

val etat_est_normalisable : etat -> bool

val normaliser_colonnes :card list FArray.t ->  int  FArray.t ->int -> (card list FArray.t *int  FArray.t)

val normaliser_etat : etat -> etat 

val est_sommet_colonnes : int ->card list FArray.t -> bool

val numero_colonnes : int ->  card list FArray.t -> int

val est_dans_registre : int-> card list -> bool

val enlever_registre : int-> card list->card list

val numero_colonne_vide :  card list FArray.t -> int 

val enlever_sommet : card list -> card list

val ajouter_element_colonne : int -> card list -> card list

val coup_valid : coup -> etat -> (int -> etat -> bool)-> (int -> etat -> bool)-> (int -> int -> bool)-> bool

val traiter_coup :  coup -> etat -> (int  -> etat -> etat)-> (int  -> etat -> etat) -> (int -> int -> etat -> etat) -> etat

val partie_gagnee : etat -> bool

val compare_state : etat -> etat -> int

val gen_source  : etat -> int list

val gen_destination : etat -> bool -> int list

val fusion  : int list -> int list -> coup list 

val calcul_score : etat ->int

val max_etats :  etat list -> int

val coup_to_string : coup-> string

val affiche_historique : etat -> unit 





