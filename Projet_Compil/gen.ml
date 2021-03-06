(*
#use "use.ml";;
open Gen;;
open Lang;;
*)

(* Compilation functions *)

open Lang
open Analyses
open Instrs


(* ************************************************************ *)
(* **** Compilation of expressions / statements            **** *)
(* ************************************************************ *)




(* ************************************************************ *)
(* **** Compilation of methods / programs                  **** *)
(* ************************************************************ *)

let gen_prog (Prog (gvds, fdfs)) =
  JVMProg ([],
           [Methdefn (Methdecl (IntT, "even", [IntT]),
                      Methinfo (3, 1),
                      [Loadc (IntT, IntV 0); ReturnI IntT])])


let rec position element = function
		(e::li) -> if e=element then 0
							  else 1+(position element li)
		|_ -> failwith "erreur element non dans liste";;


let rec gen_expr liste_var = function
	Const (tp, c) -> [Loadc(tp, c)]
	|VarE(tp, Var(_, name)) -> [Loadv(tp, (position (name, tp) liste_var))]
	|BinOp(tp, op, exp1, exp2) -> (gen_expr liste_var exp1)@(gen_expr liste_var exp2)@[Bininst(tp, op)]
	| _ -> failwith "Cas non encore traites";;


(* let exp = BinOp (IntT, BArith BAmul,
	(*exp1 -> *) BinOp (IntT, BArith BAadd, VarE (IntT, Var (Local , "x")), VarE (IntT, Var (Local , "y"))),
	(*exp2 -> *) BinOp (IntT, BArith BAsub, Const(IntT, IntV 1), VarE (IntT, Var (Local , "z")))
				);;

let expEx = BinOp (IntT, BArith BAsub,
	(* x *)			VarE (IntT, Var (Local , "x")),
	(*y+2*)			BinOp(IntT, BArith BAadd,
						VarE (IntT, Var (Local , "y")),
						Const(IntT, IntV 2))
				   );;

let liste_var = [("x", IntT ); ("y", IntT ); ("z", IntT)];;


gen_expr liste_var exp;;
gen_expr liste_var expEx;;
 *)

