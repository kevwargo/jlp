package kevwargo.jlp;

import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;

import java.io.FileInputStream;

public class Main {

    public static void main(String args[]) throws Exception {
        CommandLineParser parser = new DefaultParser();
        Options opts = new Options();
        opts.addOption(
                new Option("r", "read", false, "Only read the Lisp expressions, don't eval them"));

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
