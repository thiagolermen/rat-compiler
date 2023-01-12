open Type
open Ast.AstSyntax

(* Exceptions pour la gestion des identificateurs *)
exception DoubleDeclaration of string 
exception IdentifiantNonDeclare of string 
exception MauvaiseUtilisationIdentifiant of string 

(* Exceptions pour le typage *)
(* Le premier type est le type réel, le second est le type attendu *)
exception TypeInattendu of typ * typ
exception TypesParametresInattendus of typ list * typ list
exception TypeBinaireInattendu of binaire * typ * typ (* les types sont les types réels non compatible avec les signatures connues de l'opérateur *)
exception NotAPointer

(* Utilisation illégale de return dans le programme principal *)
exception RetourDansMain

(* Erreur dans l'identifiant d'un loop lors d'un continue ou break *)
exception MauvaisNomLoop
(* Utilisation d'un break sans être dans une boucle loop*)
exception BreakSansLoop
(* Utilisation d'un continue sans être dans une boucle loop*)
exception ContinueSansLoop
