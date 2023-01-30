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
   

  (*afficher le contenu des colonnes *)
  
  let   affiche_colonnes (e : etat) : unit =
    let rec affiche_colonnes_nth i =
      Printf.printf "\nColonne numero: %d \n" i;
      affiche_pile (FArray.get e.colonnes i);
      if i>16 then
        ()
      else
        affiche_colonnes_nth (i+1)
    in
      affiche_colonnes_nth 0;;

  (*Ajouter un element à la colonne numéro n d'un Tableau*)
  let add_element x n t=
  FArray.set t n ( x::(FArray.get (t) (n))) 

  (*ajouter un element x au registres *)
  let add_element_registre (x: card)  (rg : card list ) : card list =
    x::rg
  


  
  (*initialiser les colonnes à partir d'une permutation RECUSRSION TERMINALES*)
  let rec init_colonnes_midnight_oil perm cls n=
    match perm with
    |x::l-> 
        init_colonnes_midnight_oil (l) (add_element (Card.of_num x) (n / 3) (cls)) (n+1)
    |[]-> cls
    

let  init_midnight_oil permut=
match permut with
|[]->
  {
  depot = FArray.make 4 0;
  colonnes= FArray.make (18) ([]);
  registres= [];
  historique=[] ;
  }

|_->
  {
  depot = FArray.make 4 0;
  colonnes= init_colonnes_midnight_oil (permut) (FArray.make (18) ([])) (0);
  registres= [];
  historique= [];
 }


 (*Partie chargé d'effectuer un coup*)


(* Test la possibilité d'acceder à la source*)
let source_valide_mo s e=
if s<0 || s> 51 then 
  false
else 
   est_sommet_colonnes (s) (e.colonnes)


(*Tester la possibilite d'acceder à la distination*)
let destination_valid_mo d e =
if d<0 || d>53 then
false
else
if d=52 then
false (*Il existe bel et bien une colonne vide *)
else
if d=53 then 
 false 
else
 est_sommet_colonnes d e.colonnes

(*Test condition sur les cartes *)
let cond_carte s d =
match s with
|(r,s)-> 
match d with 
|(x,y)-> 
if s = y && r=(x-1) then 
  true 
else
  false  

(*Tester la possibiliter de mettre la source au dessus de la distination *)
let coup_carte_legal_mo s d =
if d=52 then 
match Card.of_num(s) with
|(rk,st)-> 
if rk=13 then
  true
else
  false
else
if d=53 then 
true
else
cond_carte (Card.of_num s) (Card.of_num d) 



(*Coup vers une colonne vide*)
let traite_coup_vers_colonne_vide  s e=
failwith "Ne devrait jamais ce produire car coup invalide"  

(*Coup vers le registre temporaire*)
let traite_coup_vers_registre_tmp  s e=
failwith "Ne devrait jamais ce produire car coup invalide" 

(*deplacement d'une carte vers le sommet d'une colonne*)
let traite_carte_vers_carte  s d e=
let num_cl_s=numero_colonnes (s) (e.colonnes) in 
let num_cl_d=numero_colonnes (d) (e.colonnes) in 

let num_cl_s=numero_colonnes (s) (e.colonnes) in
{
depot=e.depot;
colonnes= FArray.set (FArray.set (e.colonnes) (num_cl_d)  (ajouter_element_colonne (s)(FArray.get (e.colonnes) (num_cl_d)))) (num_cl_s) (enlever_sommet(FArray.get (e.colonnes) (num_cl_s)));
registres=e.registres;
historique = (s,d)::e.historique ;

}


(*fonction qui prend une liste de coups et renvoie la liste des coups valide freeCell*)
let coups_valides_md e l  =
  let rec coups_valides_aux e l acc = 
  match l with
  |[]-> acc
  |c::l'-> 
    if Gestion_partie.coup_valid (c) (e) (source_valide_mo) (destination_valid_mo) (coup_carte_legal_mo) then
      coups_valides_aux (e) (l') (c::acc)
    else
      coups_valides_aux (e) (l') (acc)
    in
    coups_valides_aux e l []


(*fonction qui genere tout les coups possibles valide ou pas  *)
let gen_coup  e    =
  let src = Gestion_partie.gen_source e in
  let dst = Gestion_partie.gen_destination e true in
  let coups_possibles= fusion src dst in
  coups_valides_md e coups_possibles

(*fonction qui va effectuer les coups et renvoyer une liste de nouveaux etats*)
let effectuer_coups_et_normalisation_mo e l =
  let rec effectuer_coups_aux e l acc =  (*recursion terminal*)
    match l with
    |[] -> acc
    |c::l' ->
      let ne=Gestion_partie.traiter_coup (c) (e) (traite_coup_vers_colonne_vide)(traite_coup_vers_registre_tmp) (traite_carte_vers_carte) in
    let nen=Gestion_partie.normaliser_etat (ne) in 
    effectuer_coups_aux (e) (l') (nen::acc)
in
effectuer_coups_aux e l []