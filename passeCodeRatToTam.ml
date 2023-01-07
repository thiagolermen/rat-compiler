(* Module de la passe de génération de code *)

open Ast
open Type
open Tds
open Tam
open Code

type t1 = AstPlacement.programme
type t2 = string

(* analyse_code_expression : AstType.expression -> String *)
(* Paramètre e : l'expression à analyser *)
(* Transforme l'expression en une expression de type String code TAM*)
let rec analyse_code_expression e =
  match e with
  |  AstType.AppelFonction (info_ast, lp) ->
    begin
      let c = String.concat " " (List.map analyse_code_expression lp) in
      match (info_ast_to_info info_ast) with
        | InfoFun(f, _, _) -> c^call "ST" f
        | _ -> failwith "Erreur interne"
    end
  | AstType.Ident info_ast -> 
    begin
      match info_ast_to_info info_ast with
        | InfoVar(_, t, depl, reg) -> load (getTaille t) depl reg
        | InfoConst(_, n) -> loadl_int n
        | _ -> failwith "Erreur interne"
    end
  | AstType.Booleen i -> if i then loadl_int 1 else loadl_int 0
  | AstType.Entier i -> loadl_int i
  | AstType.Unaire (op, e) -> 
    begin
      let c = analyse_code_expression e in
      match op with
        | AstType.Numerateur -> c ^ pop 0 1
        | AstType.Denominateur -> c ^ pop 1 1
    end
  | AstType.Binaire (op, e1, e2) ->
    begin
      let c1 = analyse_code_expression e1 in
      let c2 = analyse_code_expression e2 in
      match op with
        | AstType.Fraction -> c1 ^ c2 ^ call "ST" "norm"
        | AstType.PlusInt -> c1 ^ c2 ^ subr "IAdd"
        | AstType.PlusRat -> c1 ^ c2 ^ call "ST" "Radd"
        | AstType.MultInt -> c1 ^ c2 ^ subr "IMul"
        | AstType.MultRat -> c1 ^ c2 ^ call "ST" "Rmul"
        | AstType.EquInt -> c1 ^ c2 ^ subr "IEq"
        | AstType.EquBool -> c1 ^ subr "B2I" ^ c2 ^ subr "B2I" ^ subr "IEq" 
        | AstType.Inf -> c1 ^ c2 ^ subr "ILss"
    end

(* analyse_code_instruction : AstPlacement.instruction -> String *)
(* Paramètre i : l'instruction à analyser *)
(* Transforme l'instruction en un String code TAM*)
let rec analyse_code_instruction i =
  match i with
    | AstPlacement.Declaration (info_ast, e) ->
      begin 
        match info_ast_to_info info_ast with
          | InfoVar(_, t, depl, reg) -> 
            let c = push (getTaille t) in
            let ce = analyse_code_expression e in
            c^ce^(store (getTaille t) depl reg)
          | _ -> failwith "Erreur interne"
      end
    | AstPlacement.Affectation (info_ast, e) -> 
      begin
        match info_ast_to_info info_ast with
          | InfoVar(_, t, dep, reg) -> analyse_code_expression e ^ loada dep reg ^ storei (getTaille t)
          | _ -> failwith "Erreur interne"
      end
    | AstPlacement.AffichageInt e -> let ce = analyse_code_expression e in (ce^(subr "Iout"))
    | AstPlacement.AffichageRat e -> let ce = analyse_code_expression e in (ce^(call "ST" "Rout"))
    | AstPlacement.AffichageBool e -> let ne = analyse_code_expression e in (ne^(subr "Bout"))
    | AstPlacement.Conditionnelle (e, b1, b2) -> 
      begin
        let ettelse = getEtiquette() in
        let ettfin = getEtiquette() in
        let ce = analyse_code_expression e in
        let cb1 = analyse_code_bloc b1 in
        let cb2 = analyse_code_bloc b2 in
        ce ^ jumpif 0 ettelse ^ cb1 ^ jump ettfin ^ label ettelse ^ cb2 ^ label ettfin
      end
    | AstPlacement.TantQue (e, b) -> 
      let ettloop = getEtiquette() in
      let ettfin = getEtiquette() in
      let ce = analyse_code_expression e in
      label ettloop ^ ce ^ jumpif 0 ettfin ^ analyse_code_bloc b ^ jump ettloop ^ label ettfin
    | AstPlacement.Retour (e, tr, tp) ->
      let ce = analyse_code_expression e in
      ce ^ return tr tp
    | AstPlacement.Empty -> ""

(* analyse_code_bloc : AstPlacement.bloc*Int -> String *)
(* Paramètre li : la liste d'instruction à analyser *)
(* Paramètre taille : la taille du bloc *)
(* Transforme le bloc en un String code TAM*)
and analyse_code_bloc (li,t) = String.concat " " (List.map analyse_code_instruction li) ^ (pop 0 t)

(* analyse_code_fonction : AstPlacement.fonction -> String *)
(* Paramètre : la fonction à analyser *)
(* Transforme la fonction en un String code TAM*)
let analyse_code_fonction (AstPlacement.Fonction (info_ast, _, bloc)) = 
  match info_ast_to_info info_ast with
    | InfoFun(f, _, _) -> 
      (* let tailleparam = List.fold_left (fun taille t -> taille + (Type.getTaille t)) 0 tp in
      let tailleretour = getTaille tf in *)
      (* label f ^ analyse_code_bloc bloc ^ if tailleretour > 0 then return 0 tailleparam else "" *)
      label f ^ analyse_code_bloc bloc ^ halt
    | _ -> failwith "Erreur interne"


(* analyser : AstPlacement.programme -> String *)
(* Paramètre : le programme à analyser *)
(* Transforme le programme en une expression de type String code TAM*)
let analyser (AstPlacement.Programme (fonctions,prog)) =
  let nf = String.concat " " (List.map analyse_code_fonction fonctions) in
  let nb = analyse_code_bloc prog in
  (getEntete() ^ nf ^ label "main" ^ nb ^ halt)