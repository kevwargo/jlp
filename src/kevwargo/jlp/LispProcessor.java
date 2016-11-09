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
import kevwargo.jlp.objects.builtins.functions.LispBuiltins_Concat;
import kevwargo.jlp.objects.builtins.functions.LispBuiltins_Print;

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
        namespace.put("print", new LispBuiltins_Print());
        namespace.put("quote", new LispBuiltins_Quote());
        namespace.put("concat", new LispBuiltins_Concat());
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
