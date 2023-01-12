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

let pathFichiersRat = "../../../../../tests/tam/avec_fonction/fichiersRat/"

(**********)
(*  TESTS *)
(**********)


(* requires ppx_expect in jbuild, and `opam install ppx_expect` *)


let%expect_test "testIntegration1" =
  runtam (pathFichiersRat^"testIntegration1.rat");
  [%expect{| 73 |}]

let%expect_test "testIntegration2" =
  runtam (pathFichiersRat^"testIntegration2.rat");
  [%expect{| -1-74 |}]

let%expect_test "testIntegration3" =
  runtam (pathFichiersRat^"testIntegration3.rat");
  [%expect{| -104 |}]

let%expect_test "testIntegration4" =
  runtam (pathFichiersRat^"testIntegration4.rat");
  [%expect{| 01010101018880101010101 |}]

let%expect_test "testLoop15" =
  runtam (pathFichiersRat^"testLoop15.rat");
  [%expect{| 24 |}]

let%expect_test "testLoop14" =
  runtam (pathFichiersRat^"testLoop14.rat");
  [%expect{| 10 |}]

let%expect_test "testLoop13" =
  runtam (pathFichiersRat^"testLoop13.rat");
  [%expect{| 5 |}]

let%expect_test "testLoop12" =
  runtam (pathFichiersRat^"testLoop12.rat");
  [%expect{| 20 |}]

let%expect_test "testLoop11" =
  runtam (pathFichiersRat^"testLoop11.rat");
  [%expect{| 10 |}]

let%expect_test "testConditionnelleOptionnelle3" =
  runtam (pathFichiersRat^"testConditionnelleOptionnelle3.rat");
  [%expect{| 1 |}]

let%expect_test "testConditionnelleOptionnelle4" =
  runtam (pathFichiersRat^"testConditionnelleOptionnelle4.rat");
  [%expect{| 0 |}]

let%expect_test "testPointeur7" =
  runtam (pathFichiersRat^"testPointeur7.rat");
  [%expect{| 6 |}]

let%expect_test "testPointeur8" =
  runtam (pathFichiersRat^"testPointeur8.rat");
  [%expect{| 3 |}]

let%expect_test "testfun1" =
  runtam (pathFichiersRat^"testfun1.rat");
  [%expect{| 1 |}]

let%expect_test "testfun2" =
  runtam (pathFichiersRat^"testfun2.rat");
  [%expect{| 7 |}]

let%expect_test "testfun3" =
  runtam (pathFichiersRat^"testfun3.rat");
  [%expect{| 10 |}]

let%expect_test "testfun4" =
  runtam (pathFichiersRat^"testfun4.rat");
  [%expect{| 10 |}]

let%expect_test "testfun5" =
  runtam (pathFichiersRat^"testfun5.rat");
  [%expect{| |}]

let%expect_test "testfun6" =
  runtam (pathFichiersRat^"testfun6.rat");
  [%expect{|truetrue|}]

let%expect_test "testfuns" =
  runtam (pathFichiersRat^"testfuns.rat");
  [%expect{| 28 |}]

let%expect_test "factrec" =
  runtam (pathFichiersRat^"factrec.rat");
  [%expect{| 120 |}]


