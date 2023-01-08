(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Tds
open Ast
open Type
open Code
open Tam

type t1 = Ast.AstPlacement.programme
type t2 = string

(* analyse_code_pointeur : AstPlacement.affectable -> (typ * String) *)
let rec analyse_code_pointeur a =
  match a with
    | AstType.Ident info_ast -> 
      begin
        match info_ast_to_info info_ast with
          | InfoVar(_, Pointeur t, dep, reg) -> (t, load 1 dep reg)
          | _ -> failwith "Erreur interne"
      end
    | AstType.Valeur v -> 
      begin
        let (t,str) = analyse_code_pointeur v in 
        match t with
          | Pointeur t -> (t, str^loadi 1)
          | _ -> failwith "Erreur interne"
      end

(* analyse_code_affectable : AstPlacement.affectable -> String *)
let analyser_code_affectable a store_in_memory =
  match a with
    | AstType.Ident info_ast ->
      begin
        match info_ast_to_info info_ast with
          | InfoConst(_, v) -> 
            if store_in_memory then 
              failwith "Erreur interne" 
            else 
              Tam.loadl_int v
          | InfoVar(_, t, dep, reg) -> 
            if store_in_memory then 
              store (getTaille t) dep reg 
            else 
              Tam.load (getTaille t) dep reg
          | _ -> failwith "Erreur interne"
      end
    | AstType.Valeur v ->
      begin
        let (t,str) = analyse_code_pointeur v in
        if store_in_memory then 
          str^storei (getTaille t)
        else 
          str^loadi (getTaille t)
      end

(* Passe AstPlacement.expression -> string *)
let rec analyser_code_expression e =
  match e with
    | AstType.AppelFonction(ia, lexpr) ->
      begin
        match info_ast_to_info ia with
          | InfoFun(name, _, _) ->
            String.concat "" (List.map analyser_code_expression lexpr)
            ^ Tam.call "ST" name
          | _ -> failwith "Erreur interne"
      end
    | AstType.Affectable a -> analyser_code_affectable a false
    | AstType.Booleen(b) -> if b then (Tam.loadl_int 1) else (Tam.loadl_int 0)
    | AstType.Entier(i) -> Tam.loadl_int i
    | AstType.Unaire(op, e) ->
      begin
        analyser_code_expression e
        ^ match op with
          | AstType.Numerateur -> Tam.pop 0 1
          | AstType.Denominateur -> Tam.pop 1 1
      end
    | AstType.Binaire(op, e1, e2) ->
      begin
        analyser_code_expression e1
        ^ analyser_code_expression e2
        ^ match op with
          | AstType.PlusInt -> Tam.subr "IAdd"
          | AstType.PlusRat -> Tam.call "ST" "RAdd"
          | AstType.MultInt -> Tam.subr "IMul"
          | AstType.MultRat -> Tam.call "ST" "RMul"
          | AstType.EquInt -> Tam.subr "IEq"
          | AstType.EquBool -> Tam.subr "IEq"
          | AstType.Inf -> Tam.subr "ILss"
          | AstType.Fraction -> ""
      end
    | AstType.Null -> subr "MVoid"
    | AstType.New t -> loadl_int (getTaille t) ^ subr "Malloc"
    | AstType.Address n -> 
      begin
        match info_ast_to_info n with
          | InfoVar (_, _, dep, reg) -> loada dep reg
          | _ -> failwith "Erreur interne"
      end
    | AstType.ConditionnelleTernaire (c, e1, e2) -> 
      begin
        let cc = analyser_code_expression c in
        let ce1 = analyser_code_expression e1 in
        let ce2 = analyser_code_expression e2 in
        let ettfalse = getEtiquette() in
        let ettfin = getEtiquette() in
        cc ^ jumpif 0 ettfalse ^ ce1 ^ jump ettfin ^ label ettfalse ^ ce2 ^ label ettfin
      end

(* Passe AstPlacement.instruction -> string *)
let rec analyser_code_instruction i  =
  match i with
    | AstPlacement.Declaration(info, e) ->
      begin
        match info_ast_to_info info with
          | InfoVar(_, t, dep, reg) ->
              Tam.push (getTaille t)
              ^ analyser_code_expression e
              ^ Tam.store (getTaille t) dep reg
          | _ -> failwith "Erreur interne"
      end
    | AstPlacement.Affectation(a, e) -> 
      begin
        let ce = analyser_code_expression e in
        let ca = analyser_code_affectable a true in
        ce^ca
      end
    | AstPlacement.AffichageInt e ->
        analyser_code_expression e
        ^ Tam.subr "IOut"
    | AstPlacement.AffichageRat e ->
        analyser_code_expression e
        ^ Tam.call "ST" "ROut"
    | AstPlacement.AffichageBool e ->
        analyser_code_expression e
        ^ Tam.subr "BOut"
    | AstPlacement.Conditionnelle(e, b1, b2) ->
      begin
        let ettsinon = getEtiquette() in
        let ettfin = getEtiquette() in
        analyser_code_expression e
        ^ Tam.jumpif 0 ettsinon
        ^ analyser_code_bloc b1
        ^ Tam.jump ettfin
        ^ ettsinon ^ "\n"
        ^ analyser_code_bloc b2
        ^ ettfin ^ "\n"
      end
    | AstPlacement.TantQue(e, b) ->
      begin
        let ettdebut = getEtiquette() in
        let ettfin = getEtiquette() in
        ettdebut ^ "\n"
        ^ analyser_code_expression e
        ^ Tam.jumpif 0 ettfin
        ^ analyser_code_bloc b
        ^ Tam.jump ettdebut
        ^ ettfin ^ "\n"
      end
    | AstPlacement.Retour(e, t, tp) ->
      begin
          analyser_code_expression e
            ^ Tam.return t tp
      end
    | AstPlacement.Empty -> ""
    | AstPlacement.ConditionnelleOptionnelle (e, b) ->
      begin
        let _ = getEtiquette() in
        let ettfin = getEtiquette() in
        analyser_code_expression e
        ^ Tam.jumpif 0 ettfin
        ^ analyser_code_bloc b
        ^ ettfin ^ "\n"
      end


(* Passe AstPlacement.bloc -> string *)
and analyser_code_bloc (li, taillevarlocales) =
(* concaténer le code des instructions li et "POP taillevarlocales" *)
String.concat "" (List.map analyser_code_instruction li)
^ (Tam.pop 0 taillevarlocales)

(* Passe AstPlacement.fonction -> string *)
let analyser_code_fonction (AstPlacement.Fonction(info, _, (li, _))) =
  match info_ast_to_info info with
    | InfoFun(name, _, _) ->
      name ^ "\n"
      ^ String.concat "" (List.map analyser_code_instruction li)
      ^ Tam.halt
    | _ -> failwith "Erreur interne"


(* Passe AstPlacement.programme -> string *)
let analyser (AstPlacement.Programme(fonctions, bloc)) =
  getEntete()
  ^ String.concat "" (List.map analyser_code_fonction fonctions)
  ^ "main\n"
  ^ analyser_code_bloc bloc
  ^ Tam.halt