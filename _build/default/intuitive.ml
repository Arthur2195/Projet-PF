open Encodage
open Chaines

(* Saisie des mots en mode T9 *)

(* Fonction pour tester qu'une exception est bien levée *)
let does_raise f =
  try let _ = f () in false
  with _ -> true


(******************************************************************************)
(*                                EXERCICE 3                                  *)
(*                                                                            *)
(******************************************************************************)

(* QUESTION 1 *)

(******************************************************************************)
(*                                                                            *)
(*      Fonction d'encodage d'une lettre  avec saisie intuitive               *)
(*                                                                            *)
(*   signature : encoder_lettre : encodage −> char −> int                     *)
(*                                                                            *)
(*   paramètres : 
      - un encodage (liste de couples (touche, liste de lettres))             *)
(*    -  la lettre qu'on veut écrire  (char)                                  *)
(*   résultat     : le numéro de la touche sur laquelle appuyer               *)
(*                                                                            *)
(******************************************************************************)

exception LettreNonTrouvee of char

let rec encoder_lettre listeCode lettre = match listeCode with
|[] -> raise (LettreNonTrouvee lettre)
|(n, l)::q -> if (List.mem lettre l) then n else encoder_lettre q lettre

(* TESTS *)
let%test _ = encoder_lettre t9_map 'a' = 2
let%test _ = encoder_lettre t9_map 'e' = 3
let%test _ = encoder_lettre t9_map 'z' = 9
let%test _ = does_raise (fun () -> encoder_lettre t9_map '?')

let%test _ = encoder_lettre stupide_map 'a' = 2
let%test _ = encoder_lettre stupide_map 'y' = 2
let%test _ = encoder_lettre stupide_map 'm' = 3
let%test _ = does_raise (fun () -> encoder_lettre t9_map 'A')

(* QUESTION 2 *)

(******************************************************************************)
(*                                                                            *)
(*      Fonction d'encodage d'un mot avec saisie intuitive                    *)
(*                                                                            *)
(*   signature : encoder_mot : encodage -> string -> int list                *)
(*                                                                            *)
(*   paramètres :                                                             *)
(*     - encodage : une liste d’associations (touche, lettres associées)      *)
(*     - mot : une chaîne de caractères à encoder (string)                   *)
(*                                                                            *)
(*   résultat :                                                               *)
(*     - une liste d'entiers représentant les touches à presser pour écrire  *)
(*       le mot avec saisie intuitive                                        *)
(*                                                                            *)
(******************************************************************************)

let rec encoder_mot encodage mot =
  match mot with
  | "" -> []
  | _ ->
    let c = mot.[0] in
    let touche = encoder_lettre encodage c in
    touche :: encoder_mot encodage (String.sub mot 1 (String.length mot - 1))

(*TESTS*)
let%test _ = encoder_mot t9_map "bonjour" = [2; 6; 6; 5; 6; 8; 7]
let%test _ = encoder_mot t9_map "chat" = [2;4;2;8]
let%test _ = does_raise (fun () -> encoder_mot t9_map "café")

(******************************************************************************)
(*                                EXERCICE 4                                  *)
(*                                                                            *)
(******************************************************************************)
type dico = Noeud of (string list * ( int * dico) list)

(* QUESTION 1 *)
let empty = Noeud(([], []))

(*QUESTION 2*)
(******************************************************************************)
(*                   Fonction d'ajout d'un mot dans un dictionnaire           *)
(*                                                                            *)
(*   signature : encodage −> dico −> string −> dico                           *)
(*   paramètres :                                                             *)
(*      - un encodage : une liste d’associations (touche, lettres associées)  *)
(*      - un dictionnaire : arbre n-aire comportant des listes de mots dans   *)
(*        les nœuds et des chiffres (touches) sur les branches                *)
(*      - un mot : une chaîne de caractères (string)                          *)                                                                  
(*                                                                            *)
(*   résultat :                                                               *)
(*      - un dictionnaire comportant le nouveau mot                           *)
(*                                                                            *)
(******************************************************************************)
let char_list_to_string l = l |> List.to_seq |> String.of_seq
let string_to_char_list s = s |> String.to_seq |> List.of_seq


let rec recherche code c ld = 
	match ld with
	|[] -> None
	|(n,d)::q -> if ((encoder_lettre code c) = n) then (Some d) else recherche code c q

let rec maj code c nouv_branche ldTmp ld =
  match ldTmp with 
  |[] -> (encoder_lettre code c , nouv_branche)::ld
  |(n,d)::q -> if ((encoder_lettre code c) = n) then ((encoder_lettre code c),nouv_branche)::q
              else (maj code c nouv_branche q ld)

let ajout code dico mot = 
  let l_mot = string_to_char_list mot in
  let rec aux dicoTmp motTmp = 
    match motTmp, dicoTmp with 
    |[], _ -> dicoTmp
    |c::qlc, Noeud((lm,ld)) -> let s = char_list_to_string l_mot in 
                              let arbre_r = match recherche code c ld with 
                              |None -> if qlc = [] then Noeud(([char_list_to_string l_mot], []))
                                        else aux empty qlc
                              |Some Noeud((lm2,ld2)) -> 
                                                        let lmCleaned = if (qlc = []) && not (List.mem s lm2) then s :: lm2 else lm2 in
                                                        let sous_arbre = aux (Noeud((lmCleaned, ld2))) qlc in
                                                        sous_arbre

                              in Noeud((lm, maj code c (aux arbre_r qlc) ld ld))
  in aux dico l_mot

let t1 = Noeud ([], [(2, Noeud (["a"], []))])
let t2 = Noeud ([], [(2, Noeud (["a"], [(9, Noeud (["az"], []))]))])
let t3 = Noeud([], [(2, Noeud (["a"], [(3, Noeud (["af"], [])); (9, Noeud (["az"], []))]))])
let t3_sym = Noeud([], [(2, Noeud (["a"], [(9, Noeud (["az"], [])); (3, Noeud (["af"], []))]))])

let%test _ = ajout t9_map empty "a" = t1
let%test _ = ajout t9_map t1 "az" = t2
let%test _ = ajout t9_map t2 "af" = t3
let%test _ = ajout t9_map t3 "az" = t3_sym
let%test _ = does_raise (fun () -> ajout t9_map t3 "café")




let read_line ic =
  let rec loop l =
    try 
      let line = input_line ic in 
      loop (line::l)
    with End_of_file -> List.rev l
  in loop [] 
let read filename =
  let file = open_in filename in
  let lines = read_line file in
  close_in file ;
  lines 
let creer_dico encodage fichier =
  let mots = read fichier in
  List.fold_left (ajout encodage) empty mots

(*autre alternative, proposée par GPT*)
let creer_dico encodage chemin =
  let ic = open_in chemin in
  let rec loop dico =
    try
      let mot = input_line ic in
      loop (ajout encodage dico mot)
    with End_of_file -> 
      close_in ic; 
      dico
  in
  loop empty