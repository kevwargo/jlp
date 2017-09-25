package kevwargo.jlp;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import kevwargo.jlp.objects.*;
import kevwargo.jlp.objects.builtins.functions.Append_F;
import kevwargo.jlp.objects.builtins.functions.Concat_F;
import kevwargo.jlp.objects.builtins.functions.Eval_F;
import kevwargo.jlp.objects.builtins.functions.List_F;
import kevwargo.jlp.objects.builtins.functions.Print_F;
import kevwargo.jlp.objects.builtins.macros.Defmacro_M;
import kevwargo.jlp.objects.builtins.macros.Defun_M;
import kevwargo.jlp.objects.builtins.macros.If_M;
import kevwargo.jlp.objects.builtins.macros.Lambda_M;
import kevwargo.jlp.objects.builtins.macros.LetStar_M;
import kevwargo.jlp.objects.builtins.macros.Let_M;
import kevwargo.jlp.objects.builtins.macros.Progn_M;
import kevwargo.jlp.objects.builtins.macros.Quote_M;
import kevwargo.jlp.objects.builtins.macros.Setq_M;
import kevwargo.jlp.parser.LispParser;
import kevwargo.jlp.utils.LispNamespace;

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
        namespace.put("lambda", new Lambda_M());
        namespace.put("setq", new Setq_M());
        namespace.put("eval", new Eval_F());
        basicNamespace = new LispNamespace(namespace);
    }

    public void process(LispParser parser) throws IOException, LispException {
        LispObject lispObject;
        while ((lispObject = parser.read()) != null) {
            System.out.println("Result: " + lispObject.eval(basicNamespace));
            // System.out.println("Result: " + lispObject);
        }
    }

}
