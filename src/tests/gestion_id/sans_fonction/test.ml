open Rat
open Compilateur
open Exceptions

exception ErreurNonDetectee

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/gestion_id/sans_fonction/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

let%test_unit "testLoop1" =
  let _ = compiler (pathFichiersRat^"testLoop1.rat") in ()

let%test_unit "testLoop2" =
  let _ = compiler (pathFichiersRat^"testLoop2.rat") in ()

let%test_unit "testLoop3" =
  let _ = compiler (pathFichiersRat^"testLoop3.rat") in ()

let%test_unit "testLoop4" =
  let _ = compiler (pathFichiersRat^"testLoop4.rat") in ()

let%test_unit "testLoop5" =
  let _ = compiler (pathFichiersRat^"testLoop5.rat") in ()

let%test_unit "testLoop6" =
  try
    let _ = compiler (pathFichiersRat^"testLoop6.rat")
    in raise ErreurNonDetectee
  with
  | MauvaisNomLoop -> ()

let%test_unit "testLoop7" =
  try
    let _ = compiler (pathFichiersRat^"testLoop7.rat")
    in raise ErreurNonDetectee
  with
  | MauvaisNomLoop -> ()

let%test_unit "testLoop8" =
  try
    let _ = compiler (pathFichiersRat^"testLoop8.rat")
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("y") -> ()

let%test_unit "testLoop9" =
  let _ = compiler (pathFichiersRat^"testLoop9.rat") in ()

let%test_unit "testLoop10" =
  let _ = compiler (pathFichiersRat^"testLoop10.rat") in ()
  
let%test_unit "testLoop11" =
  let _ = compiler (pathFichiersRat^"testLoop11.rat") in ()

let%test_unit "testLoop12" =
  let _ = compiler (pathFichiersRat^"testLoop12.rat") in ()

let%test_unit "testLoop13" =
  let _ = compiler (pathFichiersRat^"testLoop13.rat") in ()

let%test_unit "testLoop14" =
  try
    let _ = compiler (pathFichiersRat^"testLoop14.rat")
    in raise ErreurNonDetectee
  with
  | MauvaisNomLoop -> ()

let%test_unit "testLoop15" =
  try
    let _ = compiler (pathFichiersRat^"testLoop15.rat")
    in raise ErreurNonDetectee
  with
  | MauvaisNomLoop -> ()

let%test_unit "testLoop16" =
  try
    let _ = compiler (pathFichiersRat^"testLoop16.rat")
    in raise ErreurNonDetectee
  with
  | MauvaisNomLoop -> ()

let%test_unit "testConditionnelleTernaire1" =
  let _ = compiler (pathFichiersRat^"testConditionnelleTernaire1.rat") in ()

let%test_unit "testConditionnelleTernaire2" =
  let _ = compiler (pathFichiersRat^"testConditionnelleTernaire2.rat") in ()

let%test_unit "testConditionnelleTernaire3" =
  try
    let _ = compiler (pathFichiersRat^"testConditionnelleTernaire3.rat")
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("y") -> ()

let%test_unit "testConditionnelleTernaire4" =
  try
    let _ = compiler (pathFichiersRat^"testConditionnelleTernaire4.rat")
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("y") -> ()

let%test_unit "testConditionnelleTernaire5" =
  try
    let _ = compiler (pathFichiersRat^"testConditionnelleTernaire5.rat")
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("y") -> ()

let%test_unit "testConditionnelleOptionnelle1" = 
  let _ = compiler (pathFichiersRat^"testConditionnelleOptionnelle1.rat") in ()

let%test_unit "testConditionnelleOptionnelle2" = 
  let _ = compiler (pathFichiersRat^"testConditionnelleOptionnelle1.rat") in ()

let%test_unit "testConditionnelleOptionnelle3" = 
  try 
    let _ = compiler (pathFichiersRat^"testConditionnelleOptionnelle3.rat")
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("y") -> ()

let%test_unit "testPointeur1" = 
  let _ = compiler (pathFichiersRat^"testPointeur1.rat") in ()

let%test_unit "testPointeur2" = 
  let _ = compiler (pathFichiersRat^"testPointeur2.rat") in ()

let%test_unit "testPointeur3" = 
  let _ = compiler (pathFichiersRat^"testPointeur3.rat") in ()

let%test_unit "testPointeur4" = 
  let _ = compiler (pathFichiersRat^"testPointeur4.rat") in ()

let%test_unit "testPointeur5" = 
  let _ = compiler (pathFichiersRat^"testPointeur5.rat") in ()

let%test_unit "testPointeur6" = 
  let _ = compiler (pathFichiersRat^"testPointeur6.rat") in ()

let%test_unit "testAffectation1" = 
  let _ = compiler (pathFichiersRat^"testAffectation1.rat") in ()

let%test_unit "testAffectation2"= 
  try 
    let _ = compiler (pathFichiersRat^"testAffectation2.rat") 
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("y") -> ()

let%test_unit "testAffectation3" = 
  let _ = compiler (pathFichiersRat^"testAffectation3.rat") in ()

let%test_unit "testAffectation4" = 
  try 
    let _ = compiler (pathFichiersRat^"testAffectation4.rat")
    in raise ErreurNonDetectee
  with
  | MauvaiseUtilisationIdentifiant("x") -> ()

let%test_unit "testUtilisation1" = 
  let _ = compiler (pathFichiersRat^"testUtilisation1.rat") in ()

let%test_unit "testUtilisation2" = 
  let _ = compiler (pathFichiersRat^"testUtilisation2.rat") in ()

let%test_unit "testUtilisation3" = 
  try 
    let _ = compiler (pathFichiersRat^"testUtilisation3.rat")
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("y") -> ()

let%test_unit "testUtilisation10" = 
  try 
    let _ = compiler (pathFichiersRat^"testUtilisation10.rat")
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("x") -> ()

let%test_unit "testUtilisation11" = 
  try 
    let _ = compiler (pathFichiersRat^"testUtilisation11.rat")
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("z") -> ()

let%test_unit "testUtilisation12" = 
  try 
    let _ = compiler (pathFichiersRat^"testUtilisation12.rat")
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("z") -> ()

let%test_unit "testUtilisation13" = 
  try 
    let _ = compiler (pathFichiersRat^"testUtilisation13.rat")
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("z") -> ()

let%test_unit "testUtilisation14" = 
  try 
    let _ = compiler (pathFichiersRat^"testUtilisation14.rat")
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("z") -> ()

let%test_unit "testUtilisation15" = 
  try 
    let _ = compiler (pathFichiersRat^"testUtilisation15.rat")
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("z") -> ()

let%test_unit "testUtilisation16" = 
  try 
    let _ = compiler (pathFichiersRat^"testUtilisation16.rat")
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("y") -> ()

let%test_unit "testUtilisation17" = 
  try 
    let _ = compiler (pathFichiersRat^"testUtilisation17.rat")
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("y") -> ()

let%test_unit "testUtilisation18" = 
  try 
    let _ = compiler (pathFichiersRat^"testUtilisation18.rat")
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("y") -> ()

let%test_unit "testUtilisation19" = 
  try 
    let _ = compiler (pathFichiersRat^"testUtilisation19.rat")
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("y") -> ()

let%test_unit "testRecursiviteVariable" = 
  try 
    let _ = compiler (pathFichiersRat^"testRecursiviteVariable.rat")
    in raise ErreurNonDetectee
  with
  | IdentifiantNonDeclare("x") -> ()

(* Fichiers de tests de la génération de code -> doivent passer la TDS *)
open Unix
open Filename

let rec test d p_tam = 
  try 
    let file = readdir d in
    if (check_suffix file ".rat") 
    then
    (
     try
       let _ = compiler  (p_tam^file) in (); 
     with e -> print_string (p_tam^file); print_newline(); raise e;
    )
    else ();
    test d p_tam
  with End_of_file -> ()

let%test_unit "all_tam" =
  let p_tam = "../../../../../tests/tam/sans_fonction/fichiersRat/" in
  let d = opendir p_tam in
  test d p_tam
