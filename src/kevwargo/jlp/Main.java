package kevwargo.jlp;

public class Main {

    public static void main(String args[]) throws Exception {
        LispParser parser;
        if (args.length > 0) {
            parser = new LispParser(args[0]);
        } else {
            parser = new LispParser(System.in);
        }
        LispProcessor.getInstance().process(parser);
    }
    
}
