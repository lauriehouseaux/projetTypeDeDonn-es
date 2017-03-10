import java.util.Scanner;

public class MyWrapper {

    public static void main(String[] args){
	int i;
	if (args.length <5 ){
		System.out.println("Entrer cinqs entiers s'il vous plait");
	}else{

/* appel de soustraction (deux arguments entiers car on va charger la valeur 1) */
        i = testsJasmin.soustraction(Integer.parseInt(args[0]), 
					Integer.parseInt(args[1]));
        System.out.println(args[1]+" - 2 = "+ i);

	
/* appel de multiplication (cinq arguments entiers car on charge les valeurs 2, 3 et 4) */
        i = testsJasmin.multiplication(Integer.parseInt(args[0]), 
					Integer.parseInt(args[1]), 
					Integer.parseInt(args[2]),
					Integer.parseInt(args[3]),
					Integer.parseInt(args[4]));
        System.out.println("("+ args[2] + " + " + args[3]+ ") * ( 1 - "+ args[4] + ") = " + i);

	
/* appel de sousAdd (quatres arguments entiers car on charge les valeurs 2 et 3) */
        i = testsJasmin.sousAdd(Integer.parseInt(args[0]), Integer.parseInt(args[1]),Integer.parseInt(args[2]),Integer.parseInt(args[3]));
        System.out.println(args[2]+" - ("+ args[3]+ " + 2) = " + i);
	
	}
    }

}
