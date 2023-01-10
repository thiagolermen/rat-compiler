open Rat
open Compilateur
open Passe

(* Return la liste des adresses des variables d'un programme RAT *)
let getListeDep ratfile =
  let input = open_in ratfile in
  let filebuf = Lexing.from_channel input in
  try
  let ast = Parser.main Lexer.token filebuf in
  let past = CompilateurRat.calculer_placement ast in
  let listeAdresses = VerifPlacement.analyser past in
  listeAdresses
  with
  | Lexer.Error _ as e ->
      report_error ratfile filebuf "lexical error (unexpected character).";
      raise e
  | Parser.Error as e->
      report_error ratfile filebuf "syntax error.";
      raise e

(* teste si dans le fichier fichier, dans la fonction fonction (main pour programme principal)
la occ occurence de la variable var a l'adresse dep[registre]
*)
let test fichier fonction (var,occ) (dep,registre) = 
  let l = getListeDep fichier in
  let lmain = List.assoc fonction l in
  let rec aux i lmain = 
    if i=1 
    then
      let (d,r) = List.assoc var lmain in
    (d=dep && r=registre)
    else 
      aux (i-1) (List.remove_assoc var lmain)
  in aux occ lmain

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/placement/sans_fonction/fichiersRat/"
(**********)
(*  TESTS *)
(**********)

let%test "testLoop3_2" = 
  test (pathFichiersRat^"testLoop3.rat")  "main" ("z",2)  (3,"SB")

let%test "testLoop3_1" = 
  test (pathFichiersRat^"testLoop3.rat")  "main" ("y",2)  (3,"SB")

let%test "testLoop2_3" = 
  test (pathFichiersRat^"testLoop2.rat")  "main" ("z",1)  (2,"SB")

let%test "testLoop2_2" = 
  test (pathFichiersRat^"testLoop2.rat")  "main" ("y",2)  (1,"SB")

let%test "testLoop2_1" = 
  test (pathFichiersRat^"testLoop2.rat")  "main" ("y",1)  (1,"SB")

let%test "testLoop1_1" = 
  test (pathFichiersRat^"testLoop1.rat")  "main" ("x",1)  (0,"SB")

let%test "testLoop1_2" = 
  test (pathFichiersRat^"testLoop1.rat")  "main" ("y",1)  (1,"SB")

let%test "testConditionnelleOptionnelle1_1" = 
  test (pathFichiersRat^"testConditionnelleOptionnelle1.rat")  "main" ("x",1)  (0,"SB")

let%test "testConditionnelleOptionnelle1_2" = 
  test (pathFichiersRat^"testConditionnelleOptionnelle1.rat")  "main" ("x",2)  (1,"SB")

let%test "testConditionnelleOptionnelle2_1" = 
  test (pathFichiersRat^"testConditionnelleOptionnelle2.rat")  "main" ("x",1)  (0,"SB")

let%test "testConditionnelleOptionnelle2_2" = 
  test (pathFichiersRat^"testConditionnelleOptionnelle2.rat")  "main" ("x",2)  (1,"SB")

let%test "testConditionnelleOptionnelle2_3" = 
  test (pathFichiersRat^"testConditionnelleOptionnelle2.rat")  "main" ("y",1)  (1,"SB")

let%test "testConditionnelleOptionnelle2_4" = 
  test (pathFichiersRat^"testConditionnelleOptionnelle2.rat")  "main" ("x",3)  (2,"SB")

let%test "testConditionnelleOptionnelle3_1" = 
  test (pathFichiersRat^"testConditionnelleOptionnelle3.rat")  "main" ("x",1)  (0,"SB")

let%test "testConditionnelleOptionnelle3_2" = 
  test (pathFichiersRat^"testConditionnelleOptionnelle3.rat")  "main" ("x",2)  (1,"SB")

let%test "testConditionnelleOptionnelle3_3" = 
  test (pathFichiersRat^"testConditionnelleOptionnelle3.rat")  "main" ("x",3)  (1,"SB")

let%test "testPointeur1" = 
  test (pathFichiersRat^"testPointeur1.rat")  "main" ("px",1)  (0,"SB")

let%test "testPointeur2" = 
  test (pathFichiersRat^"testPointeur2.rat")  "main" ("px",1)  (0,"SB")

let%test "testPointeur3" = 
  test (pathFichiersRat^"testPointeur3.rat")  "main" ("px",1)  (0,"SB")

let%test "testPointeur4" = 
  test (pathFichiersRat^"testPointeur4.rat")  "main" ("px",1)  (0,"SB")

let%test "testPointeur5" = 
  test (pathFichiersRat^"testPointeur5.rat")  "main" ("px",1)  (0,"SB")

let%test "testPointeur6" = 
  test (pathFichiersRat^"testPointeur6.rat")  "main" ("x",1)  (0,"SB")

let%test "testPointeur6" = 
  test (pathFichiersRat^"testPointeur6.rat")  "main" ("y",1)  (1,"SB")

let%test "test1_x" = 
  test (pathFichiersRat^"test1.rat")  "main" ("x",1)  (0,"SB")

let%test "test2_x" = 
  test (pathFichiersRat^"test2.rat")  "main" ("x",1)  (0,"SB")

let%test "test2_y" = 
  test (pathFichiersRat^"test2.rat")  "main" ("y",1) (1,"SB")

let%test "test2_z" = 
  test (pathFichiersRat^"test2.rat")  "main" ("z",1)  (2 ,"SB")

let%test "test3_x" = 
  test (pathFichiersRat^"test3.rat")  "main" ("x",1)  (0, "SB")

let%test "test3_y" = 
  test (pathFichiersRat^"test3.rat")  "main" ("y",1)  (1, "SB")

let%test "test3_z" = 
  test (pathFichiersRat^"test3.rat")  "main" ("z",1)  (2, "SB")

let%test "test4_x" = 
  test (pathFichiersRat^"test4.rat")  "main" ("x",1)  (0, "SB")
  
let%test "test4_y" = 
  test (pathFichiersRat^"test4.rat")  "main" ("y",1)  (2, "SB")
  
let%test "test4_z" = 
  test (pathFichiersRat^"test4.rat")  "main" ("z",1)  (4, "SB")

let%test "test5_x" = 
  test (pathFichiersRat^"test5.rat")  "main" ("x",1)  (0, "SB")
  
let%test "test5_y" = 
  test (pathFichiersRat^"test5.rat")  "main" ("y",1)  (1, "SB")
  
let%test "test5_z" = 
  test (pathFichiersRat^"test5.rat")  "main" ("z",1)  (3, "SB")

let%test "test6_x_1" = 
  test (pathFichiersRat^"test6.rat")  "main" ("x",1)  (0, "SB")
  
let%test "test6_y_1" = 
  test (pathFichiersRat^"test6.rat")  "main" ("y",1)  (1, "SB")
  
let%test "test6_z_1" = 
  test (pathFichiersRat^"test6.rat")  "main" ("z",1)  (3, "SB")

let%test "test6_x_2" = 
  test (pathFichiersRat^"test6.rat")  "main" ("x",2)  (4, "SB")
  
let%test "test6_y_2" = 
  test (pathFichiersRat^"test6.rat")  "main" ("y",2)  (5, "SB")
  
let%test "test6_z_2" = 
  test (pathFichiersRat^"test6.rat")  "main" ("z",2)  (7, "SB")

let%test "test6_x_3" = 
  test (pathFichiersRat^"test6.rat")  "main" ("x",3)  (4, "SB")
  
let%test "test6_y_3" = 
  test (pathFichiersRat^"test6.rat")  "main" ("y",3)  (5, "SB")
  
let%test "test6_z_3" = 
  test (pathFichiersRat^"test6.rat")  "main" ("z",3)  (7, "SB")

let%test "test6_x1" = 
  test (pathFichiersRat^"test6.rat")  "main" ("x1",1)  (4, "SB")
  
let%test "test6_y1" = 
  test (pathFichiersRat^"test6.rat")  "main" ("y1",1)  (5, "SB")
  
let%test "test6_z1" = 
  test (pathFichiersRat^"test6.rat")  "main" ("z1",1)  (7, "SB")

let%test "test7_x_1" = 
  test (pathFichiersRat^"test7.rat")  "main" ("x",1)  (0, "SB")
    
let%test "test7_y_1" = 
  test (pathFichiersRat^"test7.rat")  "main" ("y",1)  (1, "SB")
    
let%test "test7_z_1" = 
  test (pathFichiersRat^"test7.rat")  "main" ("z",1)  (3, "SB")
  
let%test "test7_x_2" = 
  test (pathFichiersRat^"test7.rat")  "main" ("x",2)  (4, "SB")
    
let%test "test7_y_2" = 
  test (pathFichiersRat^"test7.rat")  "main" ("y",2)  (5, "SB")
    
let%test "test7_z_2" = 
  test (pathFichiersRat^"test7.rat")  "main" ("z",2)  (7, "SB")
  
let%test "test7_x1" = 
  test (pathFichiersRat^"test7.rat")  "main" ("x1",1)  (4, "SB")
    
let%test "test7_y1" = 
  test (pathFichiersRat^"test7.rat")  "main" ("y1",1)  (5, "SB")
    
let%test "test7_z1" = 
  test (pathFichiersRat^"test7.rat")  "main" ("z1",1)  (7, "SB")
