package kevwargo.jlp;

import java.util.HashMap;
import java.util.List;
import java.util.ListIterator;
import kevwargo.jlp.objects.*;
import kevwargo.jlp.objects.builtins.macros.LispBuiltins_Defun;
import kevwargo.jlp.objects.builtins.macros.LispBuiltins_Defmacro;
import kevwargo.jlp.objects.builtins.macros.LispBuiltins_Let;
import kevwargo.jlp.objects.builtins.macros.LispBuiltins_LetStar;
import kevwargo.jlp.objects.builtins.macros.LispBuiltins_Quote;

public class LispProcessor {

    private static LispProcessor instance;
    private LispNamespace basicNamespace;

    public static LispProcessor getInstance() {
        if (instance == null) {
            instance = new LispProcessor();
        }
        return instance;
    }

    private LispProcessor() {
        HashMap<String, LispObject> namespace = new HashMap<String, LispObject>();
        namespace.put("print", new LispBuiltinFunction("print", new String[0], true) {
                public LispObject eval(LispNamespace namespace) {
                    for (LispObject object : rest) {
                        System.out.println(object.toString());
                    }
                    return LispNil.getInstance();
                }
            });
        namespace.put("quote", new LispBuiltins_Quote());
        namespace.put("concat", new LispBuiltinFunction("concat", new String[0], true) {
                public LispObject eval(LispNamespace namespace) throws LispException {
                    String result = "";
                    for (LispObject object : rest) {
                        if (!(object instanceof LispString)) {
                            throw new LispException("Wrong argument type: string expected");
                        }
                        result += ((LispString)object).getValue();
                    }
                    return new LispString(result);
                }
            });
        namespace.put("defun", new LispBuiltins_Defun());
        namespace.put("defmacro", new LispBuiltins_Defmacro());
        namespace.put("let", new LispBuiltins_Let());
        namespace.put("let*", new LispBuiltins_LetStar());
        basicNamespace = new LispNamespace(namespace);
    }

    public void process(String lispSource) throws LispException {
        LispObject result = (new LispParser()).parse(lispSource).eval(basicNamespace);
        System.out.println("Result: " + result.toString());
    }

}
