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
(*   signature : ajouter : encodage −> dico −> string −> dico                 *)
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

let ajouter code dico mot = 
  let l_mot = string_to_char_list mot in
  let rec aux (Noeud((lm,ld))) motTmp = match motTmp with 
    |[] -> Noeud((lm,ld))
    |c::qlc -> let arbre_r = match recherche code c ld with 
                |None -> if qlc = [] then Noeud(([char_list_to_string l_mot], []))
                            else aux empty qlc
                |Some Noeud((lm2,ld2)) -> let lmCleaned = if (qlc = []) && not (List.mem mot lm2) 
                                                            then mot :: lm2 else lm2 in
                                          let sous_arbre = aux (Noeud((lmCleaned, ld2))) qlc in
                                          sous_arbre

                in Noeud((lm, maj code c (aux arbre_r qlc) ld ld))

  in aux dico l_mot

let t1 = Noeud ([], [(2, Noeud (["a"], []))])
let t2 = Noeud ([], [(2, Noeud (["a"], [(9, Noeud (["az"], []))]))])
let t3 = Noeud([], [(2, Noeud (["a"], [(3, Noeud (["af"], [])); (9, Noeud (["az"], []))]))])
let t4 = Noeud([], [(2, Noeud (["a"], [(3, Noeud (["ae"; "af"], [])); (9, Noeud (["az"], []))]))])
let t3_sym = Noeud([], [(2, Noeud (["a"], [(9, Noeud (["az"], [])); (3, Noeud (["af"], []))]))])

let%test _ = ajouter t9_map empty "a" = t1
let%test _ = ajouter t9_map t1 "az" = t2
let%test _ = ajouter t9_map t2 "af" = t3
let%test _ = ajouter t9_map t3 "ae" = t4
let%test _ = ajouter t9_map t3 "az" = t3_sym  (*Ce test spécifique ne fonctionne pas encore*)
let%test _ = does_raise (fun () -> ajouter t9_map t3 "café")


(*QUESTION 3*)
(******************************************************************************)
(*                                                                            *)
(*      Fonction pour construire un dictionnaire                              *)
(*                                                                            *)
(*   signature : creer_dico : encodage −> string −> dico                      *)
(*                                                                            *)
(*   paramètres : 
      - un encodage (liste de couples (touche, liste de lettres))             *)
(*    -  un mot à partir d'un fichier                                         *)
(*   résultat     : un dictionnaire contenant le mot passé en parametre       *)
(*                                                                            *)
(******************************************************************************)

let creer_dico encodage fichier =
  let ic = open_in fichier in
  let rec loop dico =
    try
      let mot = input_line ic in
      loop (ajouter encodage dico mot)
    with End_of_file -> 
      close_in ic; 
      dico
  in
  loop empty

let dico_for_test = creer_dico t9_map "petit_dico_test.txt"

let%test _ =
  dico_for_test =
    Noeud
 ([],
  [(8,
    Noeud
     ([],
      [(3,
        Noeud
         ([],
          [(6,
            Noeud
             ([],
              [(3,
                Noeud
                 ([],
                  [(7,
                    Noeud
                     ([], [(3, Noeud (["tendre"; "vendre"], []))]))]))]))]))]));
   (4,
    Noeud
     ([],
      [(7,
        Noeud
         ([],
          [(4,
            Noeud ([], [(7, Noeud (["gris"], []))]));
           (6,
            Noeud ([], [(7, Noeud (["gros"], []))]))]))]));
   (2,
    Noeud
     ([],
      [(4,
        Noeud
         ([],
          [(4,
            Noeud
             ([],
              [(3,
                Noeud
                 ([], [(6, Noeud (["chien"], []))]))]));
           (2,
            Noeud
             ([],
              [(8,
                Noeud
                 (["chat"],
                  [(8,
                    Noeud
                     ([],
                      [(6,
                        Noeud
                         ([], [(6, Noeud (["chatton"], []))]))]))]))]))]));
       (2,
        Noeud
         ([],
          [(5,
            Noeud
             ([],
              [(5,
                Noeud
                 ([],
                  [(3, Noeud (["balle"], []));
                   (6,
                    Noeud
                     ([], [(6, Noeud (["ballon"], []))]))]))]))]))]))])   




(* QUESTION 4*)
(******************************************************************************)
(*            Fonction de suppression d'un mot dans un dictionnaire           *)
(*                                                                            *)
(*   signature : encodage −> dico −> string −> dico                           *)
(*   paramètres :                                                             *)
(*      - un encodage : une liste d’associations (touche, lettres associées)  *)
(*      - un dictionnaire : arbre n-aire comportant des listes de mots dans   *)
(*        les nœuds et des chiffres (touches) sur les branches                *)
(*      - un mot : une chaîne de caractères (string)                          *)                                                                  
(*                                                                            *)
(*   résultat :                                                               *)
(*      - un dictionnaire sans le mot                                         *)
(*                                                                            *)
(******************************************************************************)

let supprimer code dico mot = 
  let rec aux dicoTmp acc = match dicoTmp with
    |Noeud((lm, lc)) -> (* Ajouter le mot du noeud s'il est différent de celui à supprimer *)
                        let acc2 = List.fold_left (fun a e -> if e = mot then a else ajouter code a e) acc (List.rev lm)
                        (* Appel récursif sur tous les dicos fils *)
                        in List.fold_left (fun a (_, sousDico) -> aux sousDico a) acc2 (List.rev lc) 
  in aux dico empty


  (* TESTS *)
  let t3Bis = Noeud([], [(2, Noeud (["a"], [(3, Noeud (["ae"; "ad"], [])); (9, Noeud (["az"], []))]))])
  let t4Bis = Noeud([], [(2, Noeud (["a"], [(3, Noeud (["ae"; "af"; "ad"], [])); (9, Noeud (["az"], []))]))])

  let t5 = (ajouter t9_map t4 "sos" )
  let t6 = ajouter t9_map t5 "so"
  let%test _ = supprimer t9_map t1 "a" = empty          (*Supprimer l'unique élément d'un dictionnaire*)
  let%test _ = supprimer t9_map t2 "az" =  t1           (*Supprimer un mot à une profondeur 2 de l'arbre*)
  let%test _ = supprimer t9_map t4Bis "af" = t3Bis      (*Supprimer un mot présent dans le même noeud qu'un autre mot*)
  let%test _ = supprimer t9_map t6 "so" = t5            (*Supprimer un mot possédant des dicos fils*)
  let%test _ = supprimer t9_map t5 "sos" = t4           (*Vérifier qu'on élague bien*)
  let%test _ = supprimer t9_map t5 "bonjour" = t5       (*Supprimer un élement non présent dans le dico*)

  (*QUESTION 5*)

  let rec recherche_touche touche ld =
    match ld with
    | [] -> None
    | (n,d)::q -> if (touche = n) then (Some d) else recherche_touche touche q
    
  (******************************************************************************)
  (*                                                                            *)
  (*      Fonction qui vérifie l'appartenance d'un mot dans un dico             *)
  (*                                                                            *)
  (*   appartient : encodage −> dico −> string −> bool                          *)
  (*                                                                            *)
  (*   paramètres :                                                             *)
  (*     - un encodage (liste de couples (touche, liste de lettres))            *)
  (*    -  un dictionnaire                                                      *)
  (*    -  un mot à vérifier                                                    *)
  (*   résultat     : un booléen pour dire si le mot appartient au dictionnaire *)
  (*                                                                            *)
  (******************************************************************************)
  let appartient encodage dico mot =
    let rec chercher_dans_dico (Noeud (mots, ld)) mot_encode =
      match mot_encode with
      | [] -> List.mem mot mots 
      | t :: q ->
          match recherche_touche t ld with
          | None -> false 
          | Some sous_dico -> chercher_dans_dico sous_dico q
    in
    let mot_encode = encoder_mot encodage mot in
    chercher_dans_dico dico mot_encode
  
  let%test _ = appartient t9_map dico_for_test "ae" = false
  let%test _ = appartient t9_map dico_for_test "chat" = true
  let%test _ = appartient t9_map dico_for_test "chien" = true
  


(*QUESTION 6*)

(******************************************************************************)
(*            Fonction de vérification de cohérence de dictionnaire           *)
(*                                                                            *)
(*   signature : coherent : encodage −> dico −> bool                          *)
(*   paramètres :                                                             *)
(*      - un encodage : une liste d’associations (touche, lettres associées)  *)
(*      - un dictionnaire : arbre n-aire comportant des listes de mots dans   *)
(*        les nœuds et des chiffres (touches) sur les branches                *)                                                             
(*                                                                            *)
(*   résultat :                                                               *)
(*      - un booléen indiquant si le dictionnaire est cohérent (tous les mots *)
(*         dans un nœud sont cohérents avec le chemin qui y mène) ou non      *)
(*                                                                            *)
(******************************************************************************)

let coherent code dico = 
  let rec aux codeActuel dicoTmp = match dicoTmp with 
  |Noeud((lm, lc)) -> (List.fold_left(fun acc e -> acc && (codeActuel = encoder_mot code e)) true lm) 
                      && (List.fold_left (fun acc (n, d) -> acc && (aux (codeActuel@[n]) d))) true lc
  in aux [] dico

(* TESTS *)
let t1Err = Noeud (["b"], [])
let t4Err = Noeud([], [(2, Noeud (["a"], [(3, Noeud (["ae"; "af"; "ar"], [])); (9, Noeud (["az"], []))]))])
let t7 = Noeud ([], [(2, Noeud (["é"], []))])


let%test _ = coherent t9_map empty = true   (*Cas de base true*)
let%test _ = coherent t9_map t1Err = false  (*Cas de base false*)
let%test _ = coherent t9_map t1 = true      (*Dico de profondeur 1*)
let%test _ = coherent t9_map t2 = true      (*Dico de profondeur 2*)
let%test _ = coherent t9_map t4Bis = true
let%test _ = coherent t9_map t4Err = false

let%test _ = coherent stupide_map empty = true (*Idem avec une autre map*)
let%test _ = coherent stupide_map t2 = false
let%test _ = coherent stupide_map t6 = false
let%test _ = does_raise (fun () -> coherent t9_map t7) (*Dico qui n'est pas censé exister avec cet encodage*)


(******************************************************************************)
(*                                EXERCICE 5                                  *)
(*                                                                            *)
(******************************************************************************)
(*QUESTION 1*)
let rec decoder_mot (Noeud(lm, lc)) listTouches = match listTouches with 
|[] -> lm
|t::q -> List.fold_left (fun acc (n, d) -> if n = t then (decoder_mot d q)@acc else acc) [] lc

(*TESTS*)
let%test _ = decoder_mot dico_for_test [2;4;2;8] = ["chat"]
let%test _ = decoder_mot dico_for_test [8; 3; 6; 3; 7; 3] = ["tendre";"vendre"]

(*QUESTION 2*)
let rec prefixe (Noeud(lm, lc)) listTouches = match listTouches with
|[] -> if lc = [] then lm else List.fold_left (fun acc (_,d) -> (prefixe d [])@acc) lm lc 
|t::q -> List.fold_left (fun acc (n, d) -> if n = t then (prefixe d q)@acc else acc) [] lc

(*TESTS*)
let%test _ = prefixe dico_for_test [2] = ["ballon"; "balle"; "chatton"; "chat"; "chien"] 
let%test _ = prefixe dico_for_test [2;4] = ["chatton"; "chat"; "chien"]
let%test _ = prefixe dico_for_test [8; 3; 6] = ["tendre"; "vendre"]
