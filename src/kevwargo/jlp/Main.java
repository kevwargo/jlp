package kevwargo.jlp;

import kevwargo.jlp.parser.LispParser;


public class Main {

    public static void main(String args[]) throws Exception {
        LispParser parser;
        if (args.length > 0) {
            parser = new LispParser(args[0]);
        } else {
            parser = new LispParser(System.in);
        }
        LispProcessor proc = LispProcessor.getInstance();
        proc.process(parser);
    }

}
