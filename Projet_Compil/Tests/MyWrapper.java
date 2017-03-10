import java.util.Scanner;

public class MyWrapper {

    public static void main(String[] args){
	int i;

	/* appel de soustraction (deux arguments) */
        i = testsJasmin.soustraction(Integer.parseInt(args[0]), Integer.parseInt(args[1]));
        System.out.println("result: "+ i);

	
	/* appel de multiplication (cinq arguments) */
        i = testsJasmin.multiplication(Integer.parseInt(args[0]), 
					Integer.parseInt(args[1]), 
					Integer.parseInt(args[2]),
					Integer.parseInt(args[3]),
					Integer.parseInt(args[4]));
        System.out.println("result: "+ i);

	
	/* appel de sousAdd (quatres arguments) */
        i = testsJasmin.sousAdd(Integer.parseInt(args[0]), Integer.parseInt(args[1]),Integer.parseInt(args[2]),Integer.parseInt(args[3]));
        System.out.println("result: "+ i);

    }

}
