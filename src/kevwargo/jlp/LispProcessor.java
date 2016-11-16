package kevwargo.jlp;

import java.util.HashMap;
import java.util.List;
import java.util.Iterator;
import kevwargo.jlp.objects.*;
import kevwargo.jlp.objects.builtins.macros.Defun_M;
import kevwargo.jlp.objects.builtins.macros.Defmacro_M;
import kevwargo.jlp.objects.builtins.macros.If_M;
import kevwargo.jlp.objects.builtins.macros.Let_M;
import kevwargo.jlp.objects.builtins.macros.LetStar_M;
import kevwargo.jlp.objects.builtins.macros.Progn_M;
import kevwargo.jlp.objects.builtins.macros.Quote_M;
import kevwargo.jlp.objects.builtins.functions.Append_F;
import kevwargo.jlp.objects.builtins.functions.Concat_F;
import kevwargo.jlp.objects.builtins.functions.List_F;
import kevwargo.jlp.objects.builtins.functions.Print_F;

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
        namespace.put("print", new Print_F());
        namespace.put("quote", new Quote_M());
        namespace.put("concat", new Concat_F());
        namespace.put("defun", new Defun_M());
        namespace.put("defmacro", new Defmacro_M());
        namespace.put("let", new Let_M());
        namespace.put("let*", new LetStar_M());
        namespace.put("list", new List_F());
        namespace.put("append", new Append_F());
        namespace.put("if", new If_M());
        namespace.put("progn", new Progn_M());
        basicNamespace = new LispNamespace(namespace);
    }

    public void process(String lispSource) throws LispException {
        // LispObject result = (new LispParser()).parse(lispSource);
        LispObject result = (new LispParser()).parse(lispSource).eval(basicNamespace);
        System.out.println("Result: " + result.toString());
    }

}
