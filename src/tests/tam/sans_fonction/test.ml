open Rat
open Compilateur

(* Changer le chemin d'accès du jar. *)
let runtamcmde = "java -jar ../../../../../tests/runtam.jar"
(* let runtamcmde = "java -jar /mnt/n7fs/.../tools/runtam/runtam.jar" *)

(* Execute the TAM code obtained from the rat file and return the ouptut of this code *)
let runtamcode cmde ratfile =
  let tamcode = compiler ratfile in
  let (tamfile, chan) = Filename.open_temp_file "test" ".tam" in
  output_string chan tamcode;
  close_out chan;
  let ic = Unix.open_process_in (cmde ^ " " ^ tamfile) in
  let printed = input_line ic in
  close_in ic;
  Sys.remove tamfile;    (* à commenter si on veut étudier le code TAM. *)
  String.trim printed

(* Compile and run ratfile, then print its output *)
let runtam ratfile =
  print_string (runtamcode runtamcmde ratfile)

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/tam/sans_fonction/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

(* requires ppx_expect in jbuild, and `opam install ppx_expect` *)

let%expect_test "testLoop11" =
  runtam (pathFichiersRat^"testLoop11.rat");
  [%expect{| 30 |}]

let%expect_test "testLoop10" =
  runtam (pathFichiersRat^"testLoop10.rat");
  [%expect{| 5 |}]

let%expect_test "testLoop9" =
  runtam (pathFichiersRat^"testLoop9.rat");
  [%expect{| 5 |}]

let%expect_test "testLoop8" =
  runtam (pathFichiersRat^"testLoop8.rat");
  [%expect{| 5 |}]

let%expect_test "testLoop7" =
  runtam (pathFichiersRat^"testLoop7.rat");
  [%expect{| 10 |}]

let%expect_test "testLoop6" =
  runtam (pathFichiersRat^"testLoop6.rat");
  [%expect{| 24 |}]

let%expect_test "testLoop5" =
  runtam (pathFichiersRat^"testLoop5.rat");
  [%expect{| 20 |}]

let%expect_test "testLoop4" =
  runtam (pathFichiersRat^"testLoop4.rat");
  [%expect{| 10 |}]

let%expect_test "testLoop3" =
  runtam (pathFichiersRat^"testLoop3.rat");
  [%expect{| 10 |}]

let%expect_test "testLoop2" =
  runtam (pathFichiersRat^"testLoop2.rat");
  [%expect{| 10 |}]

let%expect_test "testLoop1" =
  runtam (pathFichiersRat^"testLoop1.rat");
  [%expect{| 10 |}]

let%expect_test "testConditionnelleOptionnelle1" =
  runtam (pathFichiersRat^"testConditionnelleOptionnelle1.rat");
  [%expect{| 1 |}]

let%expect_test "testConditionnelleOptionnelle2" =
  runtam (pathFichiersRat^"testConditionnelleOptionnelle2.rat");
  [%expect{| |}]

let%expect_test "testPointeur1" =
  runtam (pathFichiersRat^"testPointeur1.rat");
  [%expect{| 3 |}]

let%expect_test "testPointeur2" =
  runtam (pathFichiersRat^"testPointeur2.rat");
  [%expect{| [3/4] |}]

let%expect_test "testPointeur3" =
  runtam (pathFichiersRat^"testPointeur3.rat");
  [%expect{| true |}]

let%expect_test "testPointeur4" =
  runtam (pathFichiersRat^"testPointeur4.rat");
  [%expect{| 3 |}]

let%expect_test "testPointeur5" =
  runtam (pathFichiersRat^"testPointeur5.rat");
  [%expect{| 3 |}]

let%expect_test "testPointeur6" =
  runtam (pathFichiersRat^"testPointeur6.rat");
  [%expect{| 4 |}]

let%expect_test "testprintbool" =
  runtam (pathFichiersRat^"testprintbool.rat");
  [%expect{| true |}]

let%expect_test "testprintrat" =
   runtam (pathFichiersRat^"testprintrat.rat");
   [%expect{| [4/5] |}]

let%expect_test "testaddint" =
  runtam (pathFichiersRat^"testaddint.rat");
  [%expect{| 42 |}]

let%expect_test "testaddrat" =
  runtam (pathFichiersRat^"testaddrat.rat");
  [%expect{| [7/6] |}]

let%expect_test "testmultint" =
  runtam (pathFichiersRat^"testmultint.rat");
  [%expect{| 440 |}]

let%expect_test "testmultrat" =
  runtam (pathFichiersRat^"testmultrat.rat");
  [%expect{| [14/3] |}]

let%expect_test "testnum" =
  runtam (pathFichiersRat^"testnum.rat");
  [%expect{| 4 |}]

let%expect_test "testdenom" =
  runtam (pathFichiersRat^"testdenom.rat");
  [%expect{| 7 |}]

let%expect_test "testwhile1" =
  runtam (pathFichiersRat^"testwhile1.rat");
  [%expect{| 19 |}]

let%expect_test "testif1" =
  runtam (pathFichiersRat^"testif1.rat");
  [%expect{| 18 |}]

let%expect_test "testif2" =
  runtam (pathFichiersRat^"testif2.rat");
  [%expect{| 21 |}]

let%expect_test "factiter" =
  runtam (pathFichiersRat^"factiter.rat");
  [%expect{| 120 |}]

let%expect_test "complique" =
  runtam (pathFichiersRat^"complique.rat");
  [%expect{| [9/4][27/14][27/16][3/2] |}]

