open Card

type coup = int * int  (*source * destination*)

type etat =
  {
    depot : int  FArray.t; (* 0: Trefle, 1: pique, 2: coeur, 3: Carreau                          *)
    colonnes : card list FArray.t ;
    registres :  card list ;
    historique : coup list ;
  }
  (*
  colonne vide 52
  reg tmp 53
     
  
  *)

  

(*DEBUT Fonctions d'affichage utile pour le debogage*)
(*afficher le contenu d'un depot*)
let affiche_depot e =
  FArray.iter (fun n -> print_string (string_of_int (n)^ " ")) e.depot
(*afficher le contenu d'un registre*)
let affiche_registre_num e =

  List.iter (fun n -> print_string ((string_of_int (Card.to_num  (n))^ " "))) e.registres

(*afficher le contenu d'un registre en indiquant les cartes par leur numero*)
let affiche_registre e =

  List.iter (fun x -> print_string (Card.to_string   x ^ " ")) e.registres

(* afficher le contenu d'une pile *)
let  affiche_pile p =
List.iter (fun n -> print_string ((Card.to_string  (n))^ " ")) (p)
(* afficher le contenu d'une pile au format entier non convertit*)
let  affiche_pile_num p =
List.iter (fun n -> print_string ((string_of_int (Card.to_num  (n))^ " "))) (p)

(*afficher le contenu des colonnes *)
let   affiche_colonnes (e : etat) : unit =
  let rec affiche_colonnes_nth i =
    Printf.printf "\nColonne numero: %d \n" i;
    affiche_pile (FArray.get e.colonnes i);
    if i<(FArray.length (e.colonnes)-1) then
      affiche_colonnes_nth (i+1)
    else
      ()
  in
    affiche_colonnes_nth 0;;

(*afficher le contenu des colonnes en numero *)
let   affiche_colonnes_num (e : etat) : unit =
let rec affiche_colonnes_nth i =
  Printf.printf "\nColonne numero: %d \n" i;
  affiche_pile_num (FArray.get e.colonnes i);
  if i<(FArray.length (e.colonnes)-1) then
    affiche_colonnes_nth (i+1)
  else
    ()
in
  affiche_colonnes_nth 0;;
(*fonction qui affiche un etat*)
let affiche_etat e =
  begin
  print_string "affichage depot : "; affiche_depot e;
  print_string "affichage registre : "; affiche_registre e;
  print_string "\naffichage colonne : ";affiche_colonnes e;
  
  print_string "\n";
  end

(*fonction qui affiche un coup*)
let affiche_coup (c:coup) =
  match c with
  |(src,dst) -> print_string ("src: " ^ Card.to_string (Card.of_num(src) )^"  dst: "^Card.to_string( Card.of_num(dst))^"\n")
    

(*FIN Fonctions d'affichages *)



(*DBUT DE LA PARTIE NORMALISATION "commune a toutes les configurations de jeux"*)

(*verifie si il est possible de mettre une carte au depot *)
let verif_depot_carte  carte depot =
match carte with 
|(x,y)-> 
  if y=Trefle then
    if x = ((FArray.get  depot 0) +1) then 
      true
    else
      false
  else if y=Pique then
    if x = ((FArray.get  depot 1) +1) then
      true
    else
      false 
  else if y=Coeur then
    if x = ((FArray.get  depot 2) +1) then
      true
    else
      false
  else
    if x = ((FArray.get  depot 3) +1) then
      true
    else
      false
  (*vmettre une carte au depot *)
let mise_en_depot_carte  carte depot =
match carte with 
| (x,y)-> 
  if y=Trefle then
    if x = ((FArray.get  depot 0) +1) then 
      FArray.set  depot 0 x
    else
      depot
  else if y=Pique then
    if x = ((FArray.get  depot 1) +1) then
      FArray.set  depot 1 x
    else
      depot 
  else if y=Coeur then
    if x = ((FArray.get  depot 2) +1) then
      FArray.set  depot 2 x
    else
      depot
  else
    if x = ((FArray.get  depot 3) +1) then
      FArray.set  depot 3 x
    else
      depot
(* verifie si une colonne est vide *)
let est_vide cl =
  if List.length (cl)==0 then 
    true
  else 
    false
 
(*Normaliser une colonne *)
let normaliser_colonne cl depot = 
    if not (est_vide(cl))   then
        match cl with 
        |x::l -> 
          if verif_depot_carte x depot then
            ((mise_en_depot_carte  x depot) ,l)
          else
            (depot,cl)
        |_->(depot,cl)

    else 
      (depot,cl)
(* Normaliser les registre temporaires*)
let rec normaliser_registres  (ancien_reg : card list)   (nouveau_reg : card list)   (depot : int  FArray.t) : (int  FArray.t * card list) = (*(int  FArray.t * card list)*)
  match ancien_reg with 
  |x::l -> if verif_depot_carte (x) (depot) then 
              normaliser_registres  (l)   (nouveau_reg)  (mise_en_depot_carte  x depot)
            else
                  normaliser_registres  (l)   (x::nouveau_reg)  (depot)
  |[]-> (depot , nouveau_reg)


(* Verifie si une colonne est normalisable *)
let  verif_colonne_normalisable  e  cl =
  match cl with
  |x::l-> if  verif_depot_carte (x) (e.depot) then
            true
          else
            false
  |[]-> false


  (* Verifie si toutes les colonnes sont normalisable*)
  let rec verif_colonnes_normalisables  cls  etat  n =
    if (n<  (FArray.length (etat.colonnes))) then 
      if verif_colonne_normalisable (etat) (FArray.get (etat.colonnes) (n))  then 
        true
      else
        verif_colonnes_normalisables (etat.colonnes) (etat) (n+1)
    else
      false

(*verifie si les registre sont normalisable*)
let rec verif_registres_normalisable  etat reg =
  match reg with 
  |x::l-> 
  if verif_depot_carte (x) (etat.depot) then
    true
  else
    verif_registres_normalisable  (etat) (l)
    |[]-> false

let etat_est_normalisable  etat =
  verif_colonnes_normalisables  (etat.colonnes)  (etat)  (0) || verif_registres_normalisable (etat) (etat.registres)


(*Normaliser toutes les colonnes *)
let rec normaliser_colonnes  (cls: card list FArray.t ) (depot: int  FArray.t ) (n : int  ) =

  if n < (FArray.length (cls)) then 
    match  normaliser_colonne (FArray.get cls n) (depot) with 
    |(d,c)-> normaliser_colonnes (FArray.set cls n c) (d) (n+1)
  else
    (cls,depot)
  
  (*Normaliser un etat*)
  let rec  normaliser_etat e =
    let (cls,de) = normaliser_colonnes  (e.colonnes) (e.depot) (0) in 
    let (dp, rg) = normaliser_registres (e.registres) ([]) (de) in 

    let tmp_e = {
      depot=dp;
      colonnes=cls;
      registres= rg;
      historique= e.historique;
    } 

  in 
    if etat_est_normalisable  tmp_e then 
      normaliser_etat tmp_e
    else
      tmp_e
(*FIN DE LA PARTIE NORMALISATION *)





(*DEBUT Fonctions auxilières*)
(*Fonctions auxiliaires utilisées dans les fonctions principales leur but principal est de faciliter la lecture du code *)
(*Debut fonction auxiliere*)
let est_sommet_colonnes c cls =
  let rec est_sommet_colonne c cls n =
    if n<(FArray.length (cls)) then 
      match (FArray.get cls n) with 
      |x::l-> 
        if Card.to_num(x)= c then 
        true
        else
          est_sommet_colonne c cls (n+1)
      |[]->
        est_sommet_colonne c cls (n+1)
      else 
        false
      in
      est_sommet_colonne (c) (cls) (0)

(* Renvoie le numero de la colonne ou la carte s est presente au sommet*)
let numero_colonnes c cls =
  let rec numero_colonne c cls n =
    if n<(FArray.length (cls)) then 
      match (FArray.get cls n) with 
      |x::l-> 
        if Card.to_num(x) = c then 
          n
        else
          numero_colonne c cls (n+1)
      |[]->numero_colonne c cls (n+1)
    else 
        (-1)
      in
      numero_colonne (c) (cls) (0)

(*Renvoie vrai si la carte c est dans le registre *)
let  est_dans_registre c reg =
List.exists (fun x -> Card.to_num(x) = c) (reg)



  
(*Enlever la carte c du registre UTILISATION DE LA RECCURSION TERMINAL *)
let  enlever_registre (c : int)  (reg : card list)   =
let rec enlever_registre_ter (c : int)  (reg : card list) (acc : card list) =
  match reg with
  |x::l-> 
    if Card.to_num(x)= c then 
      enlever_registre_ter (c)  (l) (acc)
    else
      enlever_registre_ter (c) (l) (x::acc) 
  |[]-> acc
    in
enlever_registre_ter (c)  (reg) ([])

let numero_colonne_vide cls =
  let rec aux cls n =
    if  n<(FArray.length (cls)) then 
      if List.length( FArray.get cls n )=0 then
        n
      else
        aux (cls) (n+1)
      else
        -1   in
          aux cls 0

(*Effectuer le deplacement entre deux cartes *)
let enlever_sommet cl =
  match cl with 
  |x::l -> l
  |[]->raise Not_found 
(*Ajouter un element à la colonne*)
let ajouter_element_colonne carte cl =
  Card.of_num(carte)::cl

(*Fin Fonctions auxiliere*)



(*FIN Fonctions auxilières*)


(*renvoie vrai si un coup est valide false sinon *)
let coup_valid (c : coup) (e : etat) (cond_src : (int -> etat -> bool)) (cond_dst: (int -> etat -> bool)) (cond_coup: (int -> int -> bool))  =
(* 3 conditions à verfifier 
        - la source est valide 
        - la destination est valide
        - le coup est legal
*)
  match c with
  |(s,d)-> 
    if (cond_src (s) (e) && cond_dst (d) (e)) && (s <> d)   then

      cond_coup (s) (d)    
  else
    false



(*Traiter un coup *)
let traiter_coup (c : coup) (e : etat) (vers_colonne_vide) (vers_reg_tmp) (vers_carte) = 
  (* 3 traitement: 
      -vers_colonne vide
      -vers registre tmp
      -vers une autre carte
  *)
  match c with
    |(s,d)-> 
      if d =52 then (*Mettre la carte s sur une colonne vide *)
        begin
        let res =vers_colonne_vide  s e in 
        res
        end
      else
        begin
        if d=53 then (*mettre la carte s dans un regitre tmp*)
          begin
            let res = vers_reg_tmp  s e in
          res
          end
        else
          begin
          let res = vers_carte  (s) (d) (e) in
          res
          end
        end
        
         
(* Renvoie true si la partie est effectivement finie*)        
let partie_gagnee e =
  if FArray.get(e.depot) (0) =13 && FArray.get(e.depot) (1) =13 && FArray.get(e.depot) (2) =13 && FArray.get(e.depot) (3) =13 then
    true
  else 
    false 
  
(*DEBUT DES FONCTIONS RELATIFS A LA PARTIE 2 *)


(*fonction qui compare si deux etat sont les meme*)
let compare_state e1 e2 =
  if (Stdlib.compare e1.registres e2.registres) = 0 then
    if (Stdlib.compare e1.colonnes e2.colonnes) = 0 then
      0
    else
      Stdlib.compare e1.colonnes e2.colonnes
  else
    Stdlib.compare e1.registres e2.registres
  

(*Declaration de l'ensemble d'etat*)
module States = Set.Make (struct type t = etat let compare = compare_state end)

(*fonction qui genere toutes les sources possibles pour un coup*)
let gen_source e =
  let rec aux e n acc =
    if n<(FArray.length (e.colonnes)) then 
      match FArray.get (e.colonnes) (n) with
      |[]->aux (e) (n+1) (acc)
      |x::l-> aux (e) (n+1) (Card.to_num(x)::acc)
    else
      acc
  in
  aux (e) (0) ([]) @ List.map Card.to_num (e.registres)
(*fonction qui genere toutes les destination eventuelle d'un coup *)
let gen_destination e utilise_reg =
  let rec aux e n acc =
    if n<(FArray.length (e.colonnes)) then 
      match FArray.get (e.colonnes) (n) with
      |[]->aux (e) (n+1) (acc)
      |x::l-> aux (e) (n+1) (Card.to_num(x)::acc)
    else
      acc
  in
  if utilise_reg then
    aux (e) (0) ([]) @ (List.map Card.to_num (e.registres))@ ([52])@([53])
  else 
    aux (e) (0) ([]) @  ([52])@([53])
(*fonction qui prend une list des cartes source et une liste des destination possible et qui renvoie le resultat*)
let fusion src dst =
  
  let rec aux_dst x l2=
  match l2 with
  |[] -> []
  |a::l2' -> (x,a)::aux_dst x l2'
  in
  let rec aux_src ls ld=
  match ls with
  |[]->[]
  |a::l'-> aux_dst a ld@aux_src l' ld
  in
   aux_src src dst
(*Fonction qui calcul le score d'un etat*)
let calcul_score e =
  (FArray.get  e.depot 0) + (FArray.get  e.depot 1) + (FArray.get  e.depot 2) + (FArray.get  e.depot 3)
(*fonction qui calcul le score maximum d'une liste d'etat*)
let max_etats le=
  let rec max_etats_aux le max=
  match le with
  |[]-> max
  |e::l' -> let v = calcul_score e in
    if v > max then
      max_etats_aux l' v
    else
      max_etats_aux l' max
    in 
    max_etats_aux le 0

(*renvoier la chaine de caractere qui correspond au coup*)
let coup_to_string c =
  match c with 
  |(x,52)-> string_of_int x ^ " V"
  |(x,53) ->string_of_int x ^ " T"
  |(x,y)  ->string_of_int x ^" "^ string_of_int y

let affiche_historique e =
  print_string "nb coups historique ";
  print_int (List.length e.historique) ; print_string "\n";
  List.iter (affiche_coup) e.historique;


 

  







