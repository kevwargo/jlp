package kevwargo.jlp;

import java.io.FileReader;
import java.io.BufferedReader;

public class Main {

    public static void main(String args[]) throws Exception {
        BufferedReader reader = new BufferedReader(new FileReader(args[0]));
        String line = null;
        StringBuffer source = new StringBuffer();
        while ((line = reader.readLine()) != null) {
            source.append(line).append('\n');
        }
        System.out.println((new LispParser()).parse(source.toString()).toString());
    }
    
}
