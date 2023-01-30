
open XpatLib
open FArray
open Fifo
open Card
open Init_freecell
open Init_seahaven
open Init_midnight_oil
open Init_baker
open Gestion_partie
open Traitement_partie







type game = Freecell | Seahaven | Midnight | Baker

type mode =
  | Check of string (* filename of a solution file to check *)
  | Search of string (* filename where to write the solution *)



  type coup = int * int 



  let print_mode m = 
    match m with
    |Check(x)-> Printf.printf "Val check:  %s:\n" x
    |Search(x)-> Printf.printf "Val search %s:\n" x
  
    


  type etat =
  {
    depot : int  FArray.t;
    colonnes : card list FArray.t ;
    registres :  card list ;
    historique : coup list  ;
  }
  





type config = { mutable game : game; mutable seed: int; mutable mode: mode }
let config = { game = Freecell; seed = 1; mode = Search "" }

let print_coup c = 
match c with 
|(x,y)-> print_string ("\n Le coup est : "^(string_of_int (x)) ^" : "^(string_of_int (y))^"\n")


let return_filename mode= 
  match mode with
    |Check(x)->  x
    |Search(x)-> x

  
    
(*fonction qui lit une ligne et renvoie le coup  *)
  let read_from_file chan =
    let lines = ref "" in
    try
     
        let line = input_line chan in
        match String.split_on_char ' ' line  with 
        | x::y::[]-> 
          if String.equal y "V" then 
            (int_of_string x,52)
          else
            if String.equal y "T" then
              (int_of_string x,53)
            else 
              (int_of_string x,int_of_string y)

        |_->  failwith "Ligne vide "
    with End_of_file ->
      close_in chan;
      raise Not_found

(* Verifie si un un fichier file contient les bons coups pour resoudre la partie freecell*)
let  traitement_fc e chan =
  let rec traitement_fc_aux  e chan n = 
    let ne= Gestion_partie.normaliser_etat (e) in
      try
        let c=read_from_file (chan) in
          
          if Gestion_partie.coup_valid (c) (ne) (Init_freecell.source_valide_fr) (Init_freecell.destination_valid_fr) (Init_freecell.coup_carte_legal_fr)    then
            traitement_fc_aux (Gestion_partie.traiter_coup (c) (ne) (Init_freecell.traite_coup_vers_colonne_vide_fr)(Init_freecell.traite_coup_vers_registre_tmp_fr) (Init_freecell.traite_carte_vers_carte_fr) ) (chan) (n+1) 
          else
            
            print_string ("ECHEC "^string_of_int (n));
            exit 1
            
      with Not_found ->
        let ne= Gestion_partie.normaliser_etat (e) in
        (*Init_freecell.affiche_colonnes_num ne;*)
        if not (Gestion_partie.partie_gagnee (ne)) then 
          begin
          print_string ("ECHEC "^string_of_int (n));
          exit 1
          end
        else
          print_string "SUCCES\n";
          exit 0
        in 
        traitement_fc_aux e chan 1

(* Verifie si un un fichier file contient les bons coups pour resoudre la partie seahaven*)
let  traitement_st e chan =
  let rec traitement_st_aux  e chan n = 
    let ne= Gestion_partie.normaliser_etat (e) in
      try
        let c=read_from_file (chan) in
          
          if Gestion_partie.coup_valid (c) (ne) (Init_seahaven.source_valide_st) (Init_seahaven.destination_valid_st) (Init_seahaven.coup_carte_legal_st)    then
            traitement_st_aux (Gestion_partie.traiter_coup (c) (ne) (Init_seahaven.traite_coup_vers_colonne_vide)(Init_seahaven.traite_coup_vers_registre_tmp) (Init_seahaven.traite_carte_vers_carte) ) (chan) (n+1) 
          else
            print_string ("ECHEC "^string_of_int (n));
            exit 1
      with Not_found ->
        let ne= Gestion_partie.normaliser_etat (e) in
        (*Init_freecell.affiche_colonnes_num ne;*)
        if not (Gestion_partie.partie_gagnee (ne)) then 
          begin
            print_string ("ECHEC "^string_of_int (n));
            exit 1
          end
        else
          print_string "SUCCES\n";
          exit 0
        in 
        traitement_st_aux e chan 1


(* Verifie si un un fichier file contient les bons coups pour resoudre la partie midnight oil*)
let  traitement_mo e chan =
  let rec traitement_mo_aux  e chan n = 
    let ne= Gestion_partie.normaliser_etat (e) in
      try
        let c=read_from_file (chan) in
          
          if Gestion_partie.coup_valid (c) (ne) (Init_midnight_oil.source_valide_mo) (Init_midnight_oil.destination_valid_mo) (Init_midnight_oil.coup_carte_legal_mo)    then
            traitement_mo_aux (Gestion_partie.traiter_coup (c) (ne) (Init_midnight_oil.traite_coup_vers_colonne_vide)(Init_midnight_oil.traite_coup_vers_registre_tmp) (Init_midnight_oil.traite_carte_vers_carte) ) (chan) (n+1) 
          else
            print_string ("ECHEC "^string_of_int (n));
            exit 1
      with Not_found ->
        let ne= Gestion_partie.normaliser_etat (e) in
        if not (Gestion_partie.partie_gagnee (ne)) then 
          begin
            print_string ("ECHEC "^string_of_int (n));
            exit 1
          end
        else
          print_string "SUCCES\n";
          exit 0
        in 
        traitement_mo_aux e chan 1

(* Verifie si un un fichier file contient les bons coups pour resoudre la partie Bakers Dozen*)
let  traitement_bd e chan =
  let rec traitement_bd_aux  e chan n = 
    let ne= Gestion_partie.normaliser_etat (e) in
      try
        let c=read_from_file (chan) in
          
          if Gestion_partie.coup_valid (c) (ne) (Init_baker.source_valide_bd) (Init_baker.destination_valid_bd) (Init_baker.coup_carte_legal_bd)    then
            traitement_bd_aux (Gestion_partie.traiter_coup (c) (ne) (Init_baker.traite_coup_vers_colonne_vide)(Init_baker.traite_coup_vers_registre_tmp) (Init_baker.traite_carte_vers_carte) ) (chan) (n+1) 
          else
            print_string ("ECHEC "^string_of_int (n));
            exit 1
      with Not_found ->
        let ne= Gestion_partie.normaliser_etat (e) in
        
        if not (Gestion_partie.partie_gagnee (ne)) then 
          begin
            print_string ("ECHEC "^string_of_int (n));
            exit 1
          end
        else
          print_string "SUCCES\n";
          exit 0
        in 
        traitement_bd_aux e chan 1
          

        

          


     
    

let getgame = function
  | "FreeCell"|"fc" -> Freecell
  | "Seahaven"|"st" -> Seahaven
  | "MidnightOil"|"mo" -> Midnight
  | "BakersDozen"|"bd" -> Baker
  | _ -> raise Not_found

let split_on_dot name =
  match String.split_on_char '.' name with
  | [string1;string2] -> (string1,string2)
  | _ -> raise Not_found

let set_game_seed name =
  try
    let (sname,snum) = split_on_dot name in
    config.game <- getgame sname;
    config.seed <- int_of_string snum
  with _ -> failwith ("Error: <game>.<number> expected, with <game> in "^
                      "FreeCell Seahaven MidnightOil BakersDozen")



let treat_game conf =
  let permut = XpatRandom.shuffle  conf.seed in


  
  match config.mode with 
  |Check(filename)->(
    let chan = open_in (return_filename (config.mode)) in
    match config.game with
    |Freecell->
      traitement_fc (Init_freecell.init_freecell permut) (chan) ;
      exit 1
    |Seahaven-> 
      traitement_st (Init_seahaven.init_seahaven permut) (chan) ;
      exit 1
    |Midnight-> 
      traitement_mo (Init_midnight_oil.init_midnight_oil permut) (chan);
      exit 1
    |Baker-> traitement_bd (Init_baker.init_baker permut) (chan)
    )
  |Search(filename)-> (
    let chan = open_out filename in
    match config.game with 
    |Freecell->

      let e=Init_freecell.init_freecell (permut) 
      in
      begin
       Traitement_partie.recherche_solution  (e ) (Init_freecell.gen_coup_fr ) (Init_freecell.effectuer_coups_et_normalisation_fr) (chan); 
      end
        
    |Seahaven->
      let e=Init_seahaven.init_seahaven (permut) 
      in
      Traitement_partie.recherche_solution  (e ) (Init_seahaven.gen_coup ) (Init_seahaven.effectuer_coups_et_normalisation_st) (chan);
      
    |Midnight->
      
      let e=Init_midnight_oil.init_midnight_oil (permut) 
    in
    
     Traitement_partie.recherche_solution  (e ) (Init_midnight_oil.gen_coup ) (Init_midnight_oil.effectuer_coups_et_normalisation_mo) (chan);

    |Baker->
      let e=Init_baker.init_baker (permut) 
      in
       Traitement_partie.recherche_solution  (e ) (Init_baker.gen_coup_bd ) (Init_baker.effectuer_coups_et_normalisation_bd) (chan); 
  )
    







  

  
  

let main () =
  
  Arg.parse
    [("-check", String (fun filename -> config.mode <- Check filename),
        "<filename>:\tValidate a solution file");
     ("-search", String (fun filename -> config.mode <- Search filename),
        "<filename>:\tSearch a solution and write it to a solution file")]
    set_game_seed (* pour les arguments seuls, sans option devant *)
    "XpatSolver <game>.<number> : search solution for Xpat2 game <number>";
    
  treat_game config

let _ = if not !Sys.interactive then main () else ()
