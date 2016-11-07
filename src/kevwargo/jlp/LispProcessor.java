package kevwargo.jlp;

import java.util.HashMap;
import java.util.List;
import kevwargo.jlp.object.*;

public class LispProcessor {

    private static LispProcessor instance;
    private HashMap<String, LispObject> namespace;

    public static LispProcessor getInstance() {
        if (instance == null) {
            instance = new LispProcessor();
        }
        return instance;
    }
    
    private LispProcessor() {
        namespace = new HashMap<String, LispObject>();
        namespace.put("print", new LispFunction("print", new String[0], true) {
                public LispObject eval(HashMap<String, LispObject> namespace) {
                    for (LispObject object : rest) {
                        System.out.println(object.toString());
                    }
                    return new LispNil();
                }
            });
        namespace.put("quote", new LispMacro("quote", new String[] {"arg"}, false) {
                public LispObject eval(HashMap<String, LispObject> namespace) throws LispException {
                    return arguments.get("arg");
                }
            });
    }

    public void process(String lispSource) throws LispException {
        LispObject result = (new LispParser()).parse(lispSource).eval(namespace);
        System.out.println("Result: " + result.toString());
    }

}
