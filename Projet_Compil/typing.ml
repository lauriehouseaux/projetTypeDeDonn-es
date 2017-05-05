(*
#use "use.ml";;
open Typing;;
open Lang;;
*)


(* Typechecking of source programs *)

open Lang;;
open Analyses;;

(* Environments *)

type environment =
    {localvar: (vname * tp) list;
     globalvar: (vname * tp) list;
     returntp: tp;
     funbind: fundecl list};;


let express1 = BinOp (0,
          BCompar BCeq ,
              VarE (0, Var (Local , "n")),
              BinOp (0,
                BArith BAadd ,
                    VarE (0, Var (Local , "k")),
                    Const (0, IntV 1))) ;;

let express2 = BinOp (0,
                BArith BAsub ,
                  VarE (0, Var (Local , "n")),
                  Const (0, IntV 2)) ;;

let env = { localvar = [("k", IntT ); ("n", IntT )];
            globalvar = [];
            returntp = VoidT ;
            funbind = [Fundecl (IntT , "f", [ Vardecl (IntT , "n"); Vardecl (BoolT , "b")])]} ;;




(* ************************************************************ *)
(* ****        Exercices 1 et 2 : Fonction tp_of_expr      **** *)
(* ************************************************************ *)

exception VarNonDefinie;;
exception MalType;;
exception FonctionNonDefinie;;
exception ProblemeParametres;;


let rec tp_exp env = function

(* Exercice 1 *)
	(Const(_, b):(int expr)) -> let typeConst = function
                    (BoolV _) -> BoolT
                    |(IntV  _) -> IntT
                    |(VoidV) -> VoidT
                  in
            Const(typeConst b, b)


  |VarE(_, Var(b, nom)) -> let rec recherche = function
                              ((n, t)::liste, nom) -> if n=nom then t
                                                      else recherche(liste, nom)
                              |([], _) -> raise VarNonDefinie
                            in
            VarE(recherche(env.localvar, nom), Var(b, nom))


  |BinOp(_, operateur, exp1, exp2) -> let tpExp1 = tp_exp env exp1 in
                                      let tpExp2 = tp_exp env exp2 in
      if tp_of_expr(tpExp1) = tp_of_expr(tpExp2) then
                let tpOp = function
                  (BArith _, tp) -> if tp=IntT then tp
                                    else raise MalType
                  |(BLogic _, tp) -> if tp=BoolT then tp
                                    else raise MalType
                  |(BCompar _, tp) -> BoolT
                in
            BinOp(tpOp(operateur, tp_of_expr(tpExp1)), operateur, tpExp1, tpExp2)
      else raise MalType


  |IfThenElse(_, exp1, exp2, exp3) -> let tpExp1 = tp_exp env exp1 in
                                      let tpExp2 = tp_exp env exp2 in
                                      let tpExp3 = tp_exp env exp3 in
      if tp_of_expr(tpExp1) = BoolT && tp_of_expr(tpExp2)=tp_of_expr(tpExp3)
          then IfThenElse(tp_of_expr(tpExp2), tpExp1, tpExp2, tpExp3)
      else raise MalType


(* Exercice 2 : rajout du cas de CallE*)
  |CallE(_, fname, liste_expr) -> let rec rechercheFun = function
                                    ((Fundecl (t, fn, pds))::liste, nom) -> if fn=nom then (t, pds)
                                                            else rechercheFun(liste, nom)
                                    |([], _) -> raise FonctionNonDefinie
                                  in

              let (tpF, liste_varDecl) = rechercheFun (env.funbind, fname) in

                                  let rec compare = function
                                    (tp1::liste_varDecl, tp2::liste_expr) ->
                                      let expr_type = (tp_exp env tp2) in
                                        if tp_of_vardecl(tp1) = tp_of_expr(expr_type)
                                          then expr_type::(compare(liste_varDecl, liste_expr))
                                        else raise MalType
                                    |([],[]) -> []
                                    |(_, _) -> raise MalType
                                  in

              let liste_type = compare(liste_varDecl, liste_expr) in
              CallE(tpF, fname, liste_type) ;;


(* tp_exp env (VarE(0, Var (Local , "k")));;
tp_exp env (Const (0, IntV 1)) ;;
tp_exp env (VarE(0, Var (Local , "t")));;
tp_exp env express1;;
tp_exp env express2;;
tp_exp env (IfThenElse(0, BinOp(0,
                  BCompar BCeq,
                      Const(0, IntV 1),
                      Const(0, IntV 1)),
                  (VarE(0, Var (Local , "k"))),
                  (Const (0, IntV 1))));;

tp_exp env (CallE (0, "f", [ Const (0, IntV 3); Const (0, BoolV true )]));;
tp_exp env (CallE (0, "f", [ Const (0, IntV 3)]));;
tp_exp env (CallE (0, "f", [ Const (0, IntV 3); Const (0, IntV 4 )]));;
*)



(* Exercice 6 *)

let rec tp_stmt env = function
	Skip -> Skip
	| Assign (_, Var(b, nom), exp) -> let tpExp = tp_exp env exp in
                                    let rec recherche = function
                                      ((n, t)::liste, nom) -> if n=nom then t
                                                              else recherche(liste, nom)
                                      |([], _) -> raise VarNonDefinie
                                    in
                                    let tpVar = recherche(env.localvar, nom) in
                                      if tp_of_expr(tpExp) = tpVar then Assign(VoidT, Var(b, nom), tpExp)
                                      else raise MalType

  | Seq (stmt1 , stmt2) -> Seq((tp_stmt env stmt1), (tp_stmt env stmt2))

  | Cond (exp, stmt1, stmt2) -> let tpExp = (tp_exp env exp) in if tp_of_expr(tpExp)=BoolT then Cond(tpExp, (tp_stmt env stmt1), (tp_stmt env stmt2))
                                                                else raise MalType

  | While (exp, stmt) -> let tpExp = (tp_exp env exp) in if tp_of_expr(tpExp) = BoolT then While(tpExp, (tp_stmt env stmt))
                                                        else raise MalType

  | CallC (nom, expListe) -> let rec rechercheFunDecl = function
                                (Fundecl (t, fn, pds))::funDecls ->
                                  if fn = nom
                                  then
                                    let rec tpListe = function
                                      (exp::liste, param::paramList) ->
                                          let tpExp = tp_exp env exp in
                                            if tp_of_expr(tpExp) = tp_of_vardecl(param)
                                            then
                                            (tpExp)::(tpListe(liste,paramList))
                                            else
                                              raise MalType
                                    | ([], []) -> []
                                    | (_, _) -> raise ProblemeParametres
                                  in
                                tpListe(expListe,pds)
                              else
                                rechercheFunDecl funDecls
                      | _ -> raise FonctionNonDefinie
                      in
                  CallC(nom, rechercheFunDecl env.funbind)

  | Return (exp) -> let tpExp = tp_exp env exp in
                if tp_of_expr(tpExp) = env.returntp
                then
                  Return (tpExp)
                else
                  raise MalType
;;


(* TODO: put your definitions here *)
let tp_prog (Prog (gvds, fdfs)) =
  Prog([],
       [Fundefn (Fundecl (BoolT, "even", [Vardecl (IntT, "n")]), [], Skip)])
;;


