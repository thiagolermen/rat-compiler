(* Module de la passe de gestion des identifiants *)
(* doit être conforme à l'interface Passe *)
open Tds
open Exceptions
open Ast

type t1 = Ast.AstSyntax.programme
type t2 = Ast.AstTds.programme


(* analyse_tds_affectablen : tds -> AstSyntax.affectable -> bool -> AstTds.affectable *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre a : l'affectable à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'affectable
en une affectable de type AstTds.affectable *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_affectable tds a modif =
  match a with
    | AstSyntax.Ident n ->
    begin
      match chercherGlobalement tds n with
        | None -> raise (IdentifiantNonDeclare n)
        | Some info_ast ->
          begin
            match info_ast_to_info info_ast with
              |InfoFun _-> raise (MauvaiseUtilisationIdentifiant n)
              |InfoVar _-> AstTds.Ident info_ast
              |InfoConst _ -> if modif then raise (MauvaiseUtilisationIdentifiant n) else AstTds.Ident info_ast
              | _ -> failwith "Erreur interne"
            end
    end
    | AstSyntax.Valeur v -> AstTds.Valeur (analyse_tds_affectable tds v modif)

(* analyse_tds_expression : tds -> C -> AstTds.expression *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre e : l'expression à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'expression
en une expression de type AstTds.expression *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_expression tds e =
  match e with
    | AstSyntax.AppelFonction (nom, args) ->
      begin
        match (chercherGlobalement tds nom) with 
          | Some info_ast -> 
            begin
              match (info_ast_to_info info_ast) with 
                | InfoFun _ -> let ne = (List.map (analyse_tds_expression tds) args) in AstTds.AppelFonction(info_ast, ne)
                | _ -> raise (MauvaiseUtilisationIdentifiant (nom));
            end
          | None -> raise (IdentifiantNonDeclare (nom))
      end
    | AstSyntax.Affectable aff -> AstTds.Affectable (analyse_tds_affectable tds aff false)
    | AstSyntax.Booleen b -> AstTds.Booleen b
    | AstSyntax.Entier e -> AstTds.Entier e
    | AstSyntax.Unaire (u, e) -> AstTds.Unaire (u, analyse_tds_expression tds e)
    | AstSyntax.Binaire (b, e1, e2) -> AstTds.Binaire (b, analyse_tds_expression tds e1, analyse_tds_expression tds e2)
    | AstSyntax.Null -> AstTds.Null
    | AstSyntax.New t -> AstTds.New t
    | AstSyntax.Address n -> 
      begin 
        match chercherGlobalement tds n with
          | None -> raise (IdentifiantNonDeclare n)
          | Some info_ast ->
            begin
              match info_ast_to_info info_ast with
                | InfoFun _ -> raise (MauvaiseUtilisationIdentifiant n)
                | InfoVar _ -> AstTds.Address info_ast
                | InfoConst _ -> raise (MauvaiseUtilisationIdentifiant n)
                | _ -> failwith "Erreur interne"
            end
      end
    | AstSyntax.ConditionnelleTernaire (c, e1, e2) ->
      begin
        let nc = analyse_tds_expression tds c in
        let ne1 = analyse_tds_expression tds e1 in
        let ne2 = analyse_tds_expression tds e2 in
        AstTds.ConditionnelleTernaire (nc, ne1, ne2)
      end


(* analyse_tds_instruction : tds -> info_ast option -> AstSyntax.instruction -> AstTds.instruction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si l'instruction i est dans le bloc principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est l'instruction i sinon *)
(* Paramètre i : l'instruction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme l'instruction
en une instruction de type AstTds.instruction *)
(* Erreur si mauvaise utilisation des identifiants *)
let rec analyse_tds_instruction tds tds_loop oia ia_loop_opt i =
  match i with
  | AstSyntax.Declaration (t, n, e) ->
      begin
        match chercherLocalement tds n with
        | None ->
            (* L'identifiant n'est pas trouvé dans la tds locale,
            il n'a donc pas été déclaré dans le bloc courant *)
            (* Vérification de la bonne utilisation des identifiants dans l'expression *)
            (* et obtention de l'expression transformée *)
            let ne = analyse_tds_expression tds e in
            (* Création de l'information associée à l'identfiant *)
            let info = InfoVar (n,Undefined, 0, "") in
            (* Création du pointeur sur l'information *)
            let ia = info_to_info_ast info in
            (* Ajout de l'information (pointeur) dans la tds *)
            ajouter tds n ia;
            (* Renvoie de la nouvelle déclaration où le nom a été remplacé par l'information
            et l'expression remplacée par l'expression issue de l'analyse *)
            AstTds.Declaration (t, ia, ne)
        | Some _ ->
            (* L'identifiant est trouvé dans la tds locale,
            il a donc déjà été déclaré dans le bloc courant *)
            raise (DoubleDeclaration n)
      end
  | AstSyntax.Affectation (n,e) -> AstTds.Affectation (analyse_tds_affectable tds n true, analyse_tds_expression tds e)
  | AstSyntax.Constante (n,v) ->
      begin
        match chercherLocalement tds n with
        | None ->
          ajouter tds n (info_to_info_ast (InfoConst (n,v)));
          (* Suppression du noeud de déclaration des constantes devenu inutile *)
          AstTds.Empty
        | Some _ ->
          (* L'identifiant est trouvé dans la tds locale,
          il a donc déjà été déclaré dans le bloc courant *)
          raise (DoubleDeclaration n)
      end
  | AstSyntax.Affichage e ->
      (* Vérification de la bonne utilisation des identifiants dans l'expression *)
      (* et obtention de l'expression transformée *)
      let ne = analyse_tds_expression tds e in
      (* Renvoie du nouvel affichage où l'expression remplacée par l'expression issue de l'analyse *)
      AstTds.Affichage (ne)
  | AstSyntax.Conditionnelle (c,t,e) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc then *)
      let tast = analyse_tds_bloc tds tds_loop oia ia_loop_opt t in
      (* Analyse du bloc else *)
      let east = analyse_tds_bloc tds tds_loop oia ia_loop_opt e in
      (* Renvoie la nouvelle structure de la conditionnelle *)
      AstTds.Conditionnelle (nc, tast, east)
  | AstSyntax.TantQue (c,b) ->
      (* Analyse de la condition *)
      let nc = analyse_tds_expression tds c in
      (* Analyse du bloc *)
      let bast = analyse_tds_bloc tds tds_loop oia ia_loop_opt b in
      (* Renvoie la nouvelle structure de la boucle *)
      AstTds.TantQue (nc, bast)
  | AstSyntax.Retour (e) ->
      begin
        (* On récupère l'information associée à la fonction à laquelle le return est associée *)
        match oia with
          (* Il n'y a pas d'information -> l'instruction est dans le bloc principal : erreur *)
        | None -> raise RetourDansMain
          (* Il y a une information -> l'instruction est dans une fonction *)
        | Some ia ->
          (* Analyse de l'expression *)
          let ne = analyse_tds_expression tds e in
          AstTds.Retour (ne,ia)
      end
    | AstSyntax.Loop (n, li) -> 
      let info_ast = 
        begin
          (* Vérifie le type de Info et reivoie le info_ast associé qui a été trouvé dans la TDS*)
          match chercherGlobalement tds_loop n with
            | None -> 
              begin
                let info = InfoLoop(n, "", "") in
                info_to_info_ast info
              end 
            (* Si identificateur déjà définie, reivoi un Warning *)
            | Some _ -> let () = begin[@alert unsafe "Warning : double déclaration dans loops imbriqués"]end in 
                        let info = InfoLoop(n, "", "") in info_to_info_ast info
        end in
      ajouter tds_loop n info_ast;
      (* Analyse du bloc *)
      let nli = analyse_tds_bloc tds tds_loop oia info_ast li in
      (* Renvoie la nouvelle structure de la boucle *)
      AstTds.Loop (info_ast, nli)
    | AstSyntax.Break n ->
      begin
        if (n <> "") then
          begin
            (* Retourne le info trouvé dans la TDS - raise Exception si None*)
            match chercherGlobalement tds_loop n with
            | Some info_ast -> AstTds.Break info_ast
            | _ -> raise MauvaisNomLoop
          end
        else 
          begin
            (* Retourne le info trouvé dans la TDS - raise Exception si None*)
            match info_ast_to_info ia_loop_opt with
              | InfoLoop _ ->  AstTds.Break ia_loop_opt
              | NoLoop ->  raise BreakSansLoop
              | _ -> failwith "Erreur interne"
          end
      end
    | AstSyntax.Continue n ->
      begin
        if (n <> "") then
          begin
            (* Retourne le info trouvé dans la TDS - raise Exception si None*)
            match chercherGlobalement tds_loop n with
            | Some info_ast -> AstTds.Continue info_ast
            | _ -> raise MauvaisNomLoop
          end 
        else
          begin
            (* Retourne le info trouvé dans la TDS - raise Exception si None*)
            match info_ast_to_info ia_loop_opt with
              | InfoLoop _ ->  AstTds.Continue ia_loop_opt
              | NoLoop ->  raise ContinueSansLoop
              | _ -> failwith "Erreur interne"
          end
      end


(* analyse_tds_bloc : tds -> info_ast option -> AstSyntax.bloc -> AstTds.bloc *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre oia : None si le bloc li est dans le programme principal,
                   Some ia où ia est l'information associée à la fonction dans laquelle est le bloc li sinon *)
(* Paramètre li : liste d'instructions à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le bloc en un bloc de type AstTds.bloc *)
(* Erreur si mauvaise utilisation des identifiants *)
and analyse_tds_bloc tds tds_loop oia ia_loop_opt li =
  (* Entrée dans un nouveau bloc, donc création d'une nouvelle tds locale
  pointant sur la table du bloc parent *)
  let tdsbloc = creerTDSFille tds in
  (* Analyse des instructions du bloc avec la tds du nouveau bloc.
     Cette tds est modifiée par effet de bord *)
   let nli = List.map (analyse_tds_instruction tdsbloc tds_loop oia ia_loop_opt) li in
   (* afficher_locale tdsbloc ; *) (* décommenter pour afficher la table locale *)
   nli

(* analyse_tds_parametre : AstSyntax.parametre -> AstTds.parametre *)
(* Paramètre tdsfonc : la table des symboles d'une fonction *)
(* Paramètre (ptype, pnom) : le paramètre à analyser (son type, son nom) *)
(* Vérie que le nom du paramètre est unique et crée une infoVar associé  *)
(* Erreur si double déclaration du paramètre *)
let analyse_tds_parametre tdsfonc (ptype, pnom) = 
  match chercherLocalement tdsfonc pnom with 
    | None -> 
      begin
        let info  = InfoVar(pnom, ptype, 0, "") in
        let pia = info_to_info_ast info in
        ajouter tdsfonc pnom pia;
        (ptype, pia)
      end      
    | Some _ -> raise (DoubleDeclaration pnom)

(* analyse_tds_fonction : tds -> AstSyntax.fonction -> AstTds.fonction *)
(* Paramètre tds : la table des symboles courante *)
(* Paramètre : la fonction à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme la fonction
en une fonction de type AstTds.fonction *)
(* Erreur si double déclaration de parametre *)
let analyse_tds_fonction maintds tds_loop (AstSyntax.Fonction(t,n,lp,li))  =
      match chercherLocalement maintds n with
        | None ->
          begin
            let tdsfonc = creerTDSFille maintds in
            let lt = fst (List.split lp) in
            let info = InfoFun(n, t, lt) in
            let ia = info_to_info_ast info in
            ajouter maintds n ia;
            ajouter tdsfonc n ia;
            let nlp = List.map (analyse_tds_parametre tdsfonc) lp in
            let nli = analyse_tds_bloc tdsfonc tds_loop (Some ia) (info_to_info_ast NoLoop) li in
            AstTds.Fonction(t, ia, nlp , nli)
          end
        | Some _ -> raise (DoubleDeclaration n)


(* analyser : AstSyntax.programme -> AstTds.programme *)
(* Paramètre : le programme à analyser *)
(* Vérifie la bonne utilisation des identifiants et tranforme le programme
en un programme de type AstTds.programme *)
let analyser (AstSyntax.Programme (fonctions,prog)) =
  let tds = creerTDSMere () in
  let tds_loop = creerTDSMere () in
  let nf = List.map (analyse_tds_fonction tds tds_loop) fonctions in
  let nb = analyse_tds_bloc tds tds_loop None (info_to_info_ast NoLoop) prog in
  AstTds.Programme (nf,nb)
