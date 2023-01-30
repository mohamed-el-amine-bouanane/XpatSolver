open Card
open Gestion_partie


(*type etat =
  {
    depot : int  FArray.t; (* 0: Trefle, 1: pique, 2: coeur, 3: Carreau                          *)
    colonnes : card list FArray.t ;
    registres :  card list ;
  }
  type coup = int * int *)




(*Ajouter un element à la colonne numéro n d'un Tableau*)
let add_element x n t=
FArray.set t n ( x::(FArray.get (t) (n)))

(*ajouter un element x au registre *)
let add_element_registre x  rg=
x::rg


(*initialiser les colonnes à partir d'une permutation RECUSRSION TERMINALES*)
let rec init_colonne perm clns n m=
  match perm with
  |x::l-> 
    if n<6  then
      init_colonne (l) (add_element (of_num x) (m) (clns)) (n+1) (m)
    else
        if m mod 2 = 0 then
          init_colonne (l) (add_element (of_num x) (m) (clns)) (0) (m+1)
        else
          init_colonne (x::l) (clns) (0) (m+1)

  |[]-> clns

(*initialise les composantes d'une partie freecell et renvoie l'etat initial de la partie*)
let  init_freecell permut=
match permut with
|[]->
{
depot = FArray.make 4 0;
colonnes= FArray.make (8) ([]);
registres= [];
historique = [] ;


}

|_->
{
depot = FArray.make 4 0;
colonnes= init_colonne (permut) (FArray.make (8) ([])) (0) (0);
registres= [];
historique = [] ;
}



(*Partie chargé d'effectuer un coup*)










(* Test la possibilité d'acceder à la source*)
let source_valide_fr s e=
      if s<0 || s> 51 then 
        false
      else 
        est_dans_registre (s) (e.registres) || est_sommet_colonnes (s) (e.colonnes)      
    
(*Tester la possibilite d'acceder à la distination*)
let destination_valid_fr d e =
  if d<0 || d>53  then
   false
  else
   if d=52 then
     FArray.exists (fun x -> List.length (x)=0) (e.colonnes) (*Il existe bel et bien une colonne vide *)
   else
     if d=53 then 
       List.length (e.registres) < 4 
     else
       est_sommet_colonnes d e.colonnes     


(*Verifie la condition sur les couleur *)
let cond_couleur s d =
  if s =Trefle || s= Pique then
    d = Coeur || d= Carreau
  else
    d= Trefle || d=Pique      

(*Test condition sur les cartes *)
let cond_carte s d =
  match s with
  |(r,s)-> 
    match d with 
    |(x,y)-> 
      if (cond_couleur (s) (y)) && r=(x-1) then 
        true 
      else
        false        


(*Tester la possibiliter de mettre la source au dessus de la distination *)
let coup_carte_legal_fr s d =
  if d=52 || d=53 then 
    true
  else
    cond_carte (Card.of_num s) (Card.of_num d)      
    


(*Coup vers une colonne vide*)
  let traite_coup_vers_colonne_vide_fr  s e=
    if not (est_dans_registre  (s) (e.registres)) then 
    let num_cl_s=numero_colonnes (s) (e.colonnes) in 
    let new_cl = enlever_sommet(FArray.get (e.colonnes) (num_cl_s)) in
    let new_etat =FArray.set (e.colonnes) (num_cl_s)  (new_cl) in 
    {
      depot=e.depot;
      colonnes=FArray.set (new_etat) (numero_colonne_vide new_etat) ([Card.of_num s]);
      registres=e.registres;
      historique = (s,52)::e.historique;
    } 
    else


    {
      depot=e.depot;
      colonnes=FArray.set (e.colonnes) (numero_colonne_vide e.colonnes) ([Card.of_num s]);
      registres=enlever_registre (s) (e.registres) ;
      historique = (s,52)::e.historique ;
    }  


      


(*Coup vers le registre temporaire*)
  let traite_coup_vers_registre_tmp_fr  s e=

  let num_cl_s=numero_colonnes (s) (e.colonnes) in
  if num_cl_s = (-1) then 
   e
  else
  let new_cl = enlever_sommet(FArray.get (e.colonnes) (num_cl_s)) in
  (*print_string ("\n Le contenu de la colonne num "^ (string_of_int (num_cl_s))^" est : \n");*)

(*affiche_pile_num (new_cl);*)
    {
      depot=e.depot;
      colonnes= FArray.set (e.colonnes) (num_cl_s)  (new_cl);
      registres= (Card.of_num (s))::e.registres;
      historique = (s,53)::e.historique ;
    }
    
(*deplacement d'une carte vers le sommet d'une colonne*)
let traite_carte_vers_carte_fr  s d e=
  let num_cl_s=numero_colonnes (s) (e.colonnes) in 
  let num_cl_d=numero_colonnes (d) (e.colonnes) in 
  if est_dans_registre  (s) (e.registres) then 
    {
      depot=e.depot;
      colonnes= FArray.set (e.colonnes) (num_cl_d)  (ajouter_element_colonne (s)(FArray.get (e.colonnes) (num_cl_d)));
      registres=enlever_registre (s) (e.registres) ;
      historique = (s,d)::e.historique ;
    }
  else
    let num_cl_s=numero_colonnes (s) (e.colonnes) in
    {
      depot=e.depot;
      colonnes= FArray.set (FArray.set (e.colonnes) (num_cl_d)  (ajouter_element_colonne (s)(FArray.get (e.colonnes) (num_cl_d)))) (num_cl_s) (enlever_sommet(FArray.get (e.colonnes) (num_cl_s)));
      registres=e.registres;
      historique = (s,d)::e.historique ;


    }

(*fonction qui prend une liste de coups et renvoie la liste des coups valide freeCell*)
let coups_valide_freecell e l  =
  let rec coups_valide_freecell_aux e l acc = 
  match l with
  |[]-> acc
  |c::l'-> 
    if Gestion_partie.coup_valid (c) (e) (source_valide_fr) (destination_valid_fr) (coup_carte_legal_fr) then
      coups_valide_freecell_aux (e) (l') (c::acc)
    else
      coups_valide_freecell_aux (e) (l') (acc)
    in
    coups_valide_freecell_aux e l []





(*fonction qui genere tout les coups possibles valide ou pas  *)
let gen_coup_fr  e    =
  let src = Gestion_partie.gen_source e in
  let dst = Gestion_partie.gen_destination e true in
  let coups_possibles= fusion src dst in
  coups_valide_freecell e coups_possibles


(*fonction qui va effectuer les coups et renvoyer un liste de nouveaux etats*)
let effectuer_coups_et_normalisation_fr e l =
  let rec effectuer_coups_aux e l acc =  (*recursion terminal*)
    match l with
    |[] -> acc
    |c::l' ->
      let ne=Gestion_partie.traiter_coup (c) (e) (traite_coup_vers_colonne_vide_fr)(traite_coup_vers_registre_tmp_fr) (traite_carte_vers_carte_fr) in
    let nen=Gestion_partie.normaliser_etat (ne) in 
    effectuer_coups_aux (e) (l') (nen::acc)
in
effectuer_coups_aux e l []


