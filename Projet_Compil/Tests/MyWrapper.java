import java.util.Scanner;

public class MyWrapper {

    public static void main(String[] args){

        int i;
        i = testsJasmin1.even(Integer.parseInt(args[0]));
        System.out.println("result: "+ i);
	
	int j;
        j = testsJasmin2.even(Integer.parseInt(args[0]));
        System.out.println("result: "+ j);

	int k;
        k = testsJasmin1.even(Integer.parseInt(args[0]));
        System.out.println("result: "+ k);

    }

}
