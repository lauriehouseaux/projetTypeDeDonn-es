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


let position element = function liste -> 
	let rec aux = function 
		(el, (e::li), cpt) -> if e==el then cpt 
							  else aux(el, li, cpt+1) 
		|(_, [], _) -> failwith "erreur element non dans liste" 
	in 
	aux (element, liste, 0) ;;   

