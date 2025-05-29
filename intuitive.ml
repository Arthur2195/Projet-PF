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

