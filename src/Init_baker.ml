open Card
open Gestion_partie
  



  (*afficher le contenu d'un depot*)
  let affiche_depot e =
    FArray.iter (fun n -> print_string (string_of_int (n)^ " ")) e.depot
  (*afficher le contenu d'un registre*)
  let affiche_registre e =
  
    List.iter (fun x -> print_string (Card.to_string   x ^ " ")) e.registres

  (* afficher le contenu d'une pile (affiche la pile à l envers)*)
  let  affiche_pile p =
    List.iter (fun n -> print_string ((Card.to_string  (n))^ " ")) (p)
    (*let  rec affiche_pile_nth q i size =
      let x,q'= (pop q) in
      print_string ((Card.to_string  (x))^ " ");
      if i < (size-1) then 
        affiche_pile_nth (q') (i+1) size
    else
      ()
      
    in
      affiche_pile_nth p 0 (List.length (Fifo.to_list p));;*)
   

  (*afficher le contenu des colonnes *)
  
  let   affiche_colonnes (e : etat) : unit =
    let rec affiche_colonnes_nth i =
      Printf.printf "\nColonne numero: %d \n" i;
      affiche_pile (FArray.get e.colonnes i);
      if i>11 then
        ()
      else
        affiche_colonnes_nth (i+1)
    in
      affiche_colonnes_nth 0;;

  (*Ajouter un element à la colonne numéro n d'un Tableau*)
  let add_element x n t=
  FArray.set t n ( x::(FArray.get (t) (n))) 
  (*ajouter un element x au registre *)
  let add_element_registre (x: card)  (rg : card list ) : card list =
    x::rg


    (*Debut des fonctions qui seront en charge de descendre les rois dans les colonnes*)
  let est_roi crd=
    match crd with 
    |(r,s)-> 
      if r=13 then 
        true
      else 
        false
    
    let rec colonne_correcte cl=
      match cl with
      |x::y::l->
        if est_roi x then 
          if est_roi y then 
            colonne_correcte (y::l)
          else
            false
          else
            colonne_correcte (y::l)
      |x::[]-> true
      |[]-> true

          
    let rec descendre_roi_colonne cl =
      if colonne_correcte cl then
        cl
      else
        let rec rec_desendre_un_roi cln =
          match cln with
          |x::y::l -> 
            if ((est_roi (x)) ) then
              y::rec_desendre_un_roi(x::l)
            else
              x::rec_desendre_un_roi(y::l)
          |x::[]->[x]
          |[]->[]
        in
        descendre_roi_colonne (rec_desendre_un_roi (cl))

            
    
        let rec descendre_roi_tableau_colonne cls n=
        if n==13 then
          cls
        else
          descendre_roi_tableau_colonne(FArray.set cls n ( descendre_roi_colonne (FArray.get (cls) (n)))) (n+1)
      (*Fin des fonctions qui seront en charge de descendre les rois dans les colonnes*)
  
  (*initialiser les colonnes à partir d'une permutation RECUSRSION TERMINALES*)
  let rec init_colonnes_baker perm cls n=
    match perm with
    |x::l-> 
        init_colonnes_baker (l) (add_element (of_num x) (n / 4) (cls)) (n+1)
    |[]-> descendre_roi_tableau_colonne (cls) (0)


let  init_baker permut=
match permut with
|[]->
  {
  depot = FArray.make 4 0;
  colonnes= FArray.make (13) ([]);
  registres= [];
  historique= [];
  }

|_->
  {
  depot = FArray.make 4 0;
  colonnes= init_colonnes_baker (permut) (FArray.make (13) ([])) (0);
  registres= [];
  historique= [];
}



(*Partie chargé d'effectuer un coup*)


(* Test la possibilité d'acceder à la source*)
let source_valide_bd s e=
      if s<0 || s> 51 then 
        false
      else 
        est_dans_registre (s) (e.registres) || est_sommet_colonnes (s) (e.colonnes)


(*Tester la possibilite d'acceder à la distination*)
let destination_valid_bd d e =
  if d<0 || d>53 then
   false
  else
   if d=52 then
     FArray.exists (fun x -> List.length (x)=0) (e.colonnes)  (*Il existe bel et bien une colonne vide *)
   else
     if d=53 then 
       List.length (e.registres) < 4 
     else
       est_sommet_colonnes d e.colonnes

(*Test condition sur les cartes *)
let cond_carte s d =
  match s with
  |(r,s)-> 
    match d with 
    |(x,y)-> 
      if  r=(x-1) then 
        true 
      else
        false  

(*Tester la possibiliter de mettre la source au dessus de la distination *)
let coup_carte_legal_bd s d =
  if d=52 || d=53 then 
   false
  else
      cond_carte (Card.of_num s) (Card.of_num d) 



(*Coup vers une colonne vide*)
let traite_coup_vers_colonne_vide  s e=
    failwith "Ne devrait jamais se produire"  

(*Coup vers le registre temporaire*)
let traite_coup_vers_registre_tmp  s e=
failwith "Ne devrait jamais se produire"

(*deplacement d'une carte vers le sommet d'une colonne*)
let traite_carte_vers_carte  s d e=
let num_cl_s=numero_colonnes (s) (e.colonnes) in 
let num_cl_d=numero_colonnes (d) (e.colonnes) in 
if est_dans_registre  (s) (e.registres) then 
  {
    depot=e.depot;
    colonnes= FArray.set (e.colonnes) (num_cl_d)  (ajouter_element_colonne (s)(FArray.get (e.colonnes) (num_cl_d)));
    registres=enlever_registre (s) (e.registres) ;
    historique = (s,d)::e.historique;
  }
else
  let num_cl_s=numero_colonnes (s) (e.colonnes) in
  {
    depot=e.depot;
    colonnes= FArray.set (FArray.set (e.colonnes) (num_cl_d)  (ajouter_element_colonne (s)(FArray.get (e.colonnes) (num_cl_d)))) (num_cl_s) (enlever_sommet(FArray.get (e.colonnes) (num_cl_s)));
    registres=e.registres;
    historique = (s,d)::e.historique;


  }



  (*fonction qui prend une liste de coups et renvoie la liste des coups valide freeCell*)
let coups_valides_bd e l  =
let rec coups_valides_aux e l acc = 
match l with
|[]-> acc
|c::l'-> 
  if Gestion_partie.coup_valid (c) (e) (source_valide_bd) (destination_valid_bd) (coup_carte_legal_bd) then
    coups_valides_aux (e) (l') (c::acc)
  else
    coups_valides_aux (e) (l') (acc)
  in
  coups_valides_aux e l []


(*fonction qui genere tout les coups possibles valide ou pas  *)
let gen_coup_bd  e    =
let src = Gestion_partie.gen_source e in
let dst = Gestion_partie.gen_destination e true in
let coups_possibles= fusion src dst in
coups_valides_bd e coups_possibles

(*fonction qui va effectuer les coups et renvoyer une liste de nouveaux etats*)
let effectuer_coups_et_normalisation_bd e l =
let rec effectuer_coups_aux e l acc =  (*recursion terminal*)
  match l with
  |[] -> acc
  |c::l' ->
    let ne=Gestion_partie.traiter_coup (c) (e) (traite_coup_vers_colonne_vide)(traite_coup_vers_registre_tmp) (traite_carte_vers_carte) in
  let nen=Gestion_partie.normaliser_etat (ne) in 
  effectuer_coups_aux (e) (l') (nen::acc)
in
effectuer_coups_aux e l []