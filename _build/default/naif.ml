open Encodage
open Chaines

(* Saisie des mots sans le mode T9 *)
(* Pour marquer le passage d'un caractère à un autre, on utilise la touche 0 *)

(******************************************************************************)
(*                                EXERCICE 1                                  *)
(*                                                                            *)
(******************************************************************************)

(* QUESTION 1*)

(* Fonction pour tester qu'une exception est bien levée *)

let does_raise f =
try let _ = f () in false
with _ -> true

(******************************************************************************)
(*           Fonction pour trouver l'index d'un élement dans une liste        *)
(* signature : find_index : char -> char list -> int                          *)
(* paramètres : 
            - une lettre (char)
            - une liste de char (char list)                                   *)
(* resultat : un entier précisant la 1ère position de la lettre dans la liste, 
              en commençant à 1                                               *)

(******************************************************************************)
exception LettreNonTrouvee of char

let rec find_index lettre l = match l with
|[] -> raise (LettreNonTrouvee lettre)
|t::_ when t=lettre -> 1
|_::q -> 1 + (find_index lettre q)

(* TESTS *)
let%test _ = find_index 'a' ['a'; 'b';'c'] = 1
let%test _ = find_index 'e' ['d'; 'e'; 'f'] = 2
let%test _ = find_index 'i' ['g'; 'h'; 'i'] = 3
let%test _ = find_index 'k' ['j'; 'k'; 'k'; 'l'] = 2
let%test _ = does_raise (fun () -> find_index 'm' ['a'; 'b'; 'c'])


(******************************************************************************)
(*                                                                            *)
(*      Fonction d'encodage d'une lettre                                      *)
(*                                                                            *)
(*   signature : encoder_lettre : encodage −> char −> ( int * int )           *)
(*                                                                            *)
(*   paramètres : 
      - un encodage (liste de couples (touche, liste de lettres))             *)
(*    -  la lettre qu'on veut écrire  (char)                                  *)
(*   résultat     : un couple précisant le numéro de la touche et le nombre
de fois qu’il faut appuyer dessus pour saisir la lettre passée en paramètre   *)
(*                                                                            *)
(******************************************************************************)


let rec encoder_lettre listeCode lettre = match listeCode with
|[] -> raise (LettreNonTrouvee lettre)
|(n, l)::q -> if (List.mem lettre l) then (n, find_index lettre l) else encoder_lettre q lettre

(* TESTS *)
let%test _ = encoder_lettre t9_map 'a' = (2,1)
let%test _ = encoder_lettre t9_map 'e' = (3,2)
let%test _ = encoder_lettre t9_map 'z' = (9,4)
let%test _ = does_raise (fun () -> encoder_lettre t9_map '?')

let%test _ = encoder_lettre stupide_map 'a' = (2,1)
let%test _ = encoder_lettre stupide_map 'y' = (2,6)
let%test _ = encoder_lettre stupide_map 'm' = (3,10)
let%test _ = does_raise (fun () -> encoder_lettre t9_map 'A')


(* QUESTION 2*)

(******************************************************************************)
(*                                                                            *)
(*      Fonction d'encodage d'un caractère                                    *)
(*                                                                            *)
(*   signature : encoder_caractere : encodage -> char -> int list            *)
(*                                                                            *)
(*   paramètres :                                                             *)
(*     - un encodage : une liste de couples (touche, liste de lettres)       *)
(*     - un caractère à encoder                                               *)
(*                                                                            *)
(*   résultat :                                                               *)
(*     - une liste d’entiers représentant les touche                         *)
(*       nécessaires pour produire ce caractère, suivie d’un 0               *)
(*       (pour marquer la séparation entre les lettres)                      *)
(*                                                                            *)
(******************************************************************************)

let encoder_caractere encodage c =
  let (touche, nombre) = encoder_lettre encodage c in
  List.init nombre (fun _ -> touche) @ [0]

(*TESTS*)  
let%test _ = encoder_caractere t9_map 'a' = [2; 0]
let%test _ = encoder_caractere t9_map 'c' = [2; 2; 2; 0]
let%test _ = encoder_caractere t9_map 'f' = [3; 3; 3; 0]
let%test _ = encoder_caractere t9_map 'z' = [9; 9; 9; 9; 0]

(******************************************************************************)
(*                                                                            *)
(*      Fonction d'encodage d'un mot                                          *)
(*                                                                            *)
(*   signature : encoder_mot : encodage -> string -> int list                *)
(*                                                                            *)
(*   paramètres :                                                             *)
(*     - encodage : une liste d’associations (touche, lettres associées)     *)
(*     - mot : une chaîne de caractères à encoder (string)                   *)
(*                                                                            *)
(*   résultat :                                                               *)
(*     - une liste d'entiers représentant les touches à presser sur un       *)
(*       pour saisir le mot. Chaque lettre est convertie en                  *)
(*       une séquence de chiffres (répétée selon sa position), suivie de 0   *)
(*       pour marquer la séparation entre les lettres.                       *)
(*                                                                            *)
(******************************************************************************)

let rec encoder_mot encodage mot =
  match mot with
  | "" -> []  
  | _ ->
    let c = mot.[0] in  
    let reste = String.sub mot 1 (String.length mot - 1) in
    encoder_caractere encodage c @ encoder_mot encodage reste

(*TESTS*)
let%test _ = encoder_mot t9_map "bonjour" = [2;2;0;6;6;6;0;6;6;0;5;0;6;6;6;0;8;8;0;7;7;7;0]
let%test _ = encoder_mot t9_map "cafe" = [2;2;2;0;2;0;3;3;3;0;3;3;0]
let%test _ = encoder_mot stupide_map "chat" = [3;3;0;3;3;3;3;3;3;0;2;0;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;3;0]



(******************************************************************************)
(*                                EXERCICE 2                                  *)
(*                                                                            *)
(******************************************************************************)

(*QUESTION 1*)

(******************************************************************************)
(*                                                                            *)
(*      Fonction de décodage d'une lettre                                     *)
(*                                                                            *)
(*   signature : decoder_lettre : encodage −> int * int −> char               *)
(*                                                                            *)
(*   paramètres : 
      - un encodage (liste de couples (touche, liste de lettres))             *)
(*    -  un couple d'entier (numéro touche, nombre d'appuis)                  *)
(*   résultat     : la lettre obtenue en appuyant sur la touche 
                    le nombre de fois indiqué                                 *)
(*                                                                            *)
(******************************************************************************)
exception ToucheNonTrouvee of int

let rec decoder_lettre encodage couple = match encodage, couple with
|[], (m, _) -> raise (ToucheNonTrouvee m)
|(n, l)::q, (m, k) -> if n=m then (List.nth l (k-1)) else decoder_lettre q couple

(* TEST *)
let%test _ = decoder_lettre t9_map (2,1) = 'a'
let%test _ = decoder_lettre t9_map (3,2) = 'e'
let%test _ = decoder_lettre t9_map (9,4) = 'z'
let%test _ = does_raise (fun () -> decoder_lettre t9_map (10, 1))
let%test _ = does_raise (fun () -> decoder_lettre t9_map (2, 4))

let%test _ = decoder_lettre stupide_map (2,1) = 'a'
let%test _ = decoder_lettre stupide_map (2,6) = 'y'
let%test _ = decoder_lettre stupide_map (3,10) = 'm'
let%test _ = does_raise (fun () -> decoder_lettre stupide_map (4, 1))
let%test _ = does_raise (fun () -> decoder_lettre stupide_map (2, 7))

(* QUESTION 2 *)

(* Demander au prof si on a le droit d'utiliser cette fonction de conversion *)
let char_list_to_string l = l |> List.to_seq |> String.of_seq

(******************************************************************************)
(*                                                                            *)
(*   Fonction pour transformer une liste d'entier en liste de liste d'entier  *)
(*                                                                            *)
(*   signature : split_list_with_0 : int list -> int list list                *)
(*   résultat : Une liste de liste d'entier, splité par les "0"               *)
(*                                                                            *)
(******************************************************************************)


let split_list_with_0 listeTouche = 
  let split_el (acc, currentList) el = 
    if el = 0 then (acc@[currentList], [])
    else (acc, currentList@[el])

  in fst (List.fold_left split_el ([],[]) listeTouche)

(* TESTS *)

let%test _ = split_list_with_0 [2; 2; 0; 3; 3; 3; 0; 4; 4; 0] = [[2;2]; [3;3;3]; [4; 4]] (*Cas typique*)
let%test _ = split_list_with_0 [1; 2; 3; 0; 4; 5; 0] = [[1;2;3]; [4;5]] 
let%test _ = split_list_with_0 [1; 2; 3; 4; 5; 0] = [[1;2;3;4;5]]                       (*Avec un seul 0*)
let%test _ = split_list_with_0 [1; 2; 3; 4; 5] = []                                     (*Sans 0*)



(******************************************************************************)
(*                                                                            *)
(*      Fonction de décodage d'un mot                                         *)
(*                                                                            *)
(*   signature : decoder_mot   : encodage −> int list −> string               *)
(*                                                                            *)
(*   paramètres :                                                             *)
(*     - un encodage (liste de couples (touche, liste de lettres))            *)
(*    - une liste de touches sur lesquelles appuyer                           *)
(*                                                                            *)
(* Précondition : la liste de touche est correctement formatée, ie            *)
(* les numéros entre 2 zéros sont tous identiques,                            *)
(* la liste se termine par un 0 et aucun deux '0' consécutifs                 *)
(*                                                                            *)
(*   résultat     : un mot (string) formé par l'appui des touches             *)
(*                                                                            *)
(******************************************************************************)


let rec decoder_mot encodage listeTouche =
  (*Transforme une liste de liste en liste de couples (1er élement de la liste, nb élements dans la liste)  *)
  let list_of_couple listeTouche = List.map (fun li -> (List.hd li, List.length li)) listeTouche

  (*Decode chaque lettre de la liste obtenue par split et transformation en couple de listeTouche*)
  in char_list_to_string (List.map (decoder_lettre encodage) (list_of_couple (split_list_with_0 listeTouche)))

(* TESTS *)
let%test _ = decoder_mot t9_map [2; 2; 0; 6; 6; 6; 0; 6; 6; 0; 5; 0; 6; 6; 6; 0; 8; 8; 0; 7; 7; 7; 0] = "bonjour"
let%test _ = decoder_mot t9_map [2; 0; 2; 2; 0; 2; 2; 2; 0] = "abc"
let%test _ = does_raise (fun () -> decoder_mot t9_map [2;2;2;2;0]) (*4 appuis sur la touche 2*)
let%test _ = does_raise (fun () -> decoder_mot t9_map [10;0]) (*1 appui sur la touche 10*)

let%test _ = decoder_mot stupide_map [2; 0; 3; 0; 3; 3; 0] = "abc"
let%test _ = does_raise (fun () -> decoder_mot stupide_map [2;2;2;2;2;2;2;0]) (*7 appuis sur la touche 2*)
let%test _ = does_raise (fun () -> decoder_mot stupide_map [4;0]) (*1 appui sur la touche 4*)

