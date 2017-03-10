(*

#use "use.ml";;
open Lang;;
#use "Tests/mesTests.ml";;

*)

open Typing;;
open Gen;;
open Print_instr;;


(****************************************************************)
					(* POUR JASMIN *)
(****************************************************************)

let texte1 =  ".class testsJasmin"^"\n"
			^".super java/lang/Object"^"\n"
			^".method static even(I)I"^"\n"
			^pr_instrs 0 (gen_expr env.localvar (tp_exp env express2))
			^".end method";;

let texte2 =  ".class testsJasmin"^"\n"
			^".super java/lang/Object"^"\n"
			^".method static even(I)I"^"\n"
			^pr_instrs 0 (gen_expr env.localvar (tp_exp env express2))
			^".end method";;


let texte3 =  ".class testsJasmin"^"\n"
			^".super java/lang/Object"^"\n"
			^".method static even(I)I"^"\n"
			^pr_instrs 0 (gen_expr env.localvar (tp_exp env express2))
			^".end method";;


let ecrire fichier = function texte ->
		let fic = open_out(fichier) in (output_string fic texte); flush fic;;

ecrire "Tests/testsJasmin1.j" texte1;;
ecrire "Tests/testsJasmin2.j" texte2;;
ecrire "Tests/testsJasmin3.j" texte3;;

(* ************************************************************ *)
(* ****        Expressions servant pour les tests          **** *)
(* ************************************************************ *)


(* n = k+1 *)
let express1 = BinOp (0,
          BCompar BCeq ,
              VarE (0, Var (Local , "n")),
              BinOp (0,
                BArith BAadd ,
                    VarE (0, Var (Local , "k")),
                    Const (0, IntV 1))) ;;



(* n-2 *)
let express2 = BinOp (0,
                BArith BAsub ,
                  VarE (0, Var (Local , "n")),
                  Const (0, IntV 2)) ;;



(* (x+y)*(1-z) *)
let express3 = BinOp (0, BArith BAmul,
	(*exp1 -> *) BinOp (0, BArith BAadd, VarE (0, Var (Local , "x")), VarE (0, Var (Local , "y"))),
	(*exp2 -> *) BinOp (0, BArith BAsub, Const(0, IntV 1), VarE (0, Var (Local , "z")))
				);;



(* x-(y+2) *)
let express4 = BinOp (0, BArith BAsub,
	(* x *)			VarE (0, Var (Local , "x")),
	(*y+2*)			BinOp(0, BArith BAadd,
						VarE (0, Var (Local , "y")),
						Const(0, IntV 2))
				   );;


(* f 3 true *)
let express5 = CallE (0, "f", [ Const (0, IntV 3); Const (0, BoolV true )]);;


(* if 1=1 then k else 1 *)
let express6 = IfThenElse(0, BinOp(0,
                  BCompar BCeq,
                      Const(0, IntV 1),
                      Const(0, IntV 1)),
                  (VarE(0, Var (Local , "k"))),
                  (Const (0, IntV 1)));;



(* ************************************************************ *)
(* ****        Environnement pris pour les tests           **** *)
(* ************************************************************ *)

(* On crée un environnement avec
	- des variables locales
	- pas de variables globales car on ne s'y interesse pas pour le moment
	- une fonction f
*)


let env = { localvar = [("k", IntT ); ("n", IntT ); ("x", IntT); ("y", IntT); ("z", IntT)];
            globalvar = [];
            returntp = VoidT ;
            funbind = [Fundecl (IntT , "f", [ Vardecl (IntT , "n"); Vardecl (BoolT , "b")])]} ;;



(* ************************************************************ *)
(* ****        				 TESTS 				           **** *)
(* ************************************************************ *)


(****************************************************************)
				(* POUR LA FONCTION TP_EXP *)
(****************************************************************)

tp_exp env (VarE(0, Var (Local , "k")));;
(* - : Lang.tp Lang.expr = VarE (IntT, Var (Local, "k")) *)

tp_exp env (Const (0, IntV 1)) ;;
(* - : Lang.tp Lang.expr = Const (IntT, IntV 1) *)


tp_exp env express1;;
(*  - : Lang.tp Lang.expr =
			BinOp (BoolT, BCompar BCeq,
							VarE (IntT, Var (Local, "n")),
 							BinOp (IntT, BArith BAadd,
 								VarE (IntT, Var (Local, "k")),
  								Const (IntT, IntV 1)))
 *)


tp_exp env express2;;
(* - : Lang.tp Lang.expr =
			BinOp (IntT, BArith BAsub,
					VarE (IntT, Var (Local, "n")),
 					Const (IntT, IntV 2))
*)


tp_exp env express3;;
(* - : Lang.tp Lang.expr =
			BinOp (IntT, BArith BAmul,
 				BinOp (IntT, BArith BAadd,
 					VarE (IntT, Var (Local, "x")),
  					VarE (IntT, Var (Local, "y"))),
 				BinOp (IntT, BArith BAsub,
 					Const (IntT, IntV 1),
  					VarE (IntT, Var (Local, "z"))))
 *)



tp_exp env express4;;
(* - : Lang.tp Lang.expr =
			BinOp (IntT, BArith BAsub,
					VarE (IntT, Var (Local, "x")),
 					BinOp (IntT, BArith BAadd,
 						VarE (IntT, Var (Local, "y")),
  						Const (IntT, IntV 2)))
*)



tp_exp env express5;;
(* CallE (IntT, "f", [Const (IntT, IntV 3); Const (BoolT, BoolV true)]) *)


tp_exp env express6;;
(* - : Lang.tp Lang.expr =
			IfThenElse (IntT,
 					BinOp (BoolT, BCompar BCeq,
 						Const (IntT, IntV 1),
 						Const (IntT, IntV 1)),
 					VarE (IntT, Var (Local, "k")),
 					Const (IntT, IntV 1))
*)


(* Pour tester les exceptions : *)

tp_exp env (VarE(0, Var (Local , "t")));;
(* Exception: Typing.VarNonDefinie. *)


tp_exp env (BinOp (0,
                BArith BAsub ,
                  VarE (0, Var (Local , "n")),
                  Const (0, BoolV true)));;
(* Exception: Typing.MalType. *)




(****************************************************************)
					(* POUR LA FONCTION GEN_EXPR *)
(****************************************************************)


gen_expr env.localvar (tp_exp env express1);;
(* - : Instrs.instr list =
			[Loadv (IntT, 1); Loadv (IntT, 0); Loadc (IntT, IntV 1);
 			Bininst (IntT, BArith BAadd); Bininst (BoolT, BCompar BCeq)]
*)

gen_expr env.localvar (tp_exp env express2);;
(* [Loadv (IntT, 1); Loadc (IntT, IntV 2); Bininst (IntT, BArith BAsub)] *)

gen_expr env.localvar (tp_exp env express3);;
(* - : Instrs.instr list =
			[Loadv (IntT, 2); Loadv (IntT, 3); Bininst (IntT, BArith BAadd);
 			Loadc (IntT, IntV 1); Loadv (IntT, 4); Bininst (IntT, BArith BAsub);
 			Bininst (IntT, BArith BAmul)]
*)

gen_expr env.localvar (tp_exp env express4);;
(* - : Instrs.instr list =
	[Loadv (IntT, 2); Loadv (IntT, 3); Loadc (IntT, IntV 2);
 	Bininst (IntT, BArith BAadd); Bininst (IntT, BArith BAsub)]
 *)


(* Pour tester l'exception : *)

gen_expr env.localvar (VarE(IntT, Var (Local , "t")));;
(* Exception: Failure "erreur element non dans liste". *)


(****************************************************************)
					(* POUR GENERER DU TEXTE *)
(****************************************************************)
print_string(pr_instrs 0 (gen_expr env.localvar (tp_exp env express1)));;
print_string(pr_instrs 0 (gen_expr env.localvar (tp_exp env express2)));;
print_string(pr_instrs 0 (gen_expr env.localvar (tp_exp env express3)));;
print_string(pr_instrs 0 (gen_expr env.localvar (tp_exp env express4)));;



