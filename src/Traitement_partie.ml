open Card
(*open Init_freecell
open Init_seahaven
open Init_midnight_oil
open Init_baker*)
open Gestion_partie



(*Declaration de l'ensemble d'etat*)
module States = Set.Make (struct type t = etat let compare = compare_state end)

(*fonction qui mets à jours le tableau d'ens avec les nouveau etats*)
let rec maj_ens tab_ens ens_vue  l =
  match l with
  |[] -> tab_ens
  |e::l' -> 
    if States.mem e ens_vue  then 
      maj_ens tab_ens ens_vue  l'
  else
    let n_tab_ens= FArray.set (tab_ens) (Gestion_partie.calcul_score e) (States.add (e) (FArray.get (tab_ens) (Gestion_partie.calcul_score (e)))) in
    maj_ens n_tab_ens ens_vue  l'
    
(*fonction qui prend un etat de partie freecell genere tout les coups possibles effectue les coups et renvoie une liste des nouveaux etats normalisé*)

let etats_suivant e  generateur_coups generateur_etats =
  let lc=generateur_coups(e) in
  generateur_etats e lc


(*fonction qui genere le tableau des ensembles d'etats*)
let init_ens =
  FArray.make (53) (States.empty)







(*fonction qui parcours les etats de façon recursive jusqu'a trouver unse solution ou qu'il n'y est plus d'etats valides*)
let rec parcours_etat  (generateur_coups) (generateur_etats) (tab_ens) (ens_vue) (max)=
     
    let ens_max= FArray.get (tab_ens) (max) in

    if States.is_empty (ens_max) then 
      if max = 0 then 
         None
      else
        parcours_etat  (generateur_coups) (generateur_etats) (tab_ens) (ens_vue) (max-1)
    else
      let e_max = States.choose ens_max  
         in 

      if Gestion_partie.calcul_score(e_max)=52 then
        begin
        
        Some(e_max)
        end
      else
        
        
        let le= etats_suivant (e_max)  (generateur_coups) (generateur_etats)   in 
        
        let ntab_ens =FArray.set (tab_ens) (max) (try  States.remove e_max  ens_max with Not_found ->print_string "err"; States.empty ) in
        let m= Gestion_partie.max_etats le in 
        
        parcours_etat  (generateur_coups) (generateur_etats) (maj_ens ntab_ens ens_vue  le) (States.add e_max ens_vue) 
        (let nmax=Gestion_partie.max_etats le in 
        if nmax>max then nmax
        else
          max)
        

        



        
(*fonction qui ecrit la liste des coups dans un fichier à l'aide d'un descripeur de fichier rentrer en argument*)
let rec write_in_file chan lc =
match lc with
|[]-> close_out chan;
|c::l'->
   Printf.fprintf (chan) "%s\n" (Gestion_partie.coup_to_string (c));
   write_in_file chan l'





(*fonction qui effectue une recherce des solution*)
let recherche_solution  (e : etat) (generateur_coups) (generateur_etats) (chan)  =
let en= Gestion_partie.normaliser_etat(e) in 
let lc= generateur_coups(en)  in 
let le= generateur_etats e lc in
let max =Gestion_partie.max_etats le in 

(*List.iter (Gestion_partie.affiche_etat) (le)*)
match parcours_etat  (generateur_coups) (generateur_etats) (maj_ens (init_ens) (States.empty)  (le) ) (States.add (en) (States.empty)) (max) with
|None -> 
  print_string "INSOLUBLE";
  exit 1;
|Some(e) -> 
  print_string "SUCCES";
  write_in_file chan (List.rev e.historique);
  exit 0;
