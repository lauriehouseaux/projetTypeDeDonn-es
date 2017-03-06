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
            funbind = []} ;;




(* ************************************************************ *)
(* ****         Exercice 1 : Fonction tp_of_expr           **** *)
(* ************************************************************ *)

exception VarNonDefinie;;
exception MalType;;


let rec tp_exp env = function
	Const(0, b) -> let typeConst = function
                    (BoolV _) -> BoolT
                    |(IntV  _) -> IntT
                    | (VoidV) -> VoidT
                  in 
            Const(typeConst b, b)

  |VarE(0, Var(b, nom)) -> let rec recherche = function
                              ((n, t)::liste, nom) -> if n=nom then t
                                                      else recherche(liste, nom)
                              |([], _) -> raise VarNonDefinie
                            in
            VarE(recherche(env.localvar, nom), Var(b, nom))

  |BinOp(0, operateur, exp1, exp2) -> let tpExp1 = tp_exp env exp1 in
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

  |IfThenElse(0, exp1, exp2, exp3) -> let tpExp1 = tp_exp env exp1 in
                                      let tpExp2 = tp_exp env exp2 in
                                      let tpExp3 = tp_exp env exp3 in
      if tp_of_expr(tpExp1) = BoolT && tp_of_expr(tpExp2)=tp_of_expr(tpExp3)
          then IfThenElse(tp_of_expr(tpExp2), tpExp1, tpExp2, tpExp3)
      else raise MalType
  ;;


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
*)


(* TODO: put your definitions here *)
let tp_prog (Prog (gvds, fdfs)) =
  Prog([],
       [Fundefn (Fundecl (BoolT, "even", [Vardecl (IntT, "n")]), [], Skip)])
;;


