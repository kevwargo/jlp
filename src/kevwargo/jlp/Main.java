package kevwargo.jlp;

import java.io.FileInputStream;

public class Main {

    public static void main(String args[]) throws Exception {
        LispProcessor proc = LispProcessor.getInstance();

        if (args.length > 1) {
            int port = Integer.parseInt(args[1]);
            proc.runServer(args[0], port);
        } else if (args.length == 1) {
            proc.run(new FileInputStream(args[0]));
        } else {
            proc.runInteractive(System.in, System.out, System.err);
        }
    }
}
