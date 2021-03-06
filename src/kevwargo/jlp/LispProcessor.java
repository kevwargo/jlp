package kevwargo.jlp;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.builtins.functions.Append_F;
import kevwargo.jlp.objects.builtins.functions.Apply_F;
import kevwargo.jlp.objects.builtins.functions.Concat_F;
import kevwargo.jlp.objects.builtins.functions.Divide_F;
import kevwargo.jlp.objects.builtins.functions.Eq_F;
import kevwargo.jlp.objects.builtins.functions.Equalp_F;
import kevwargo.jlp.objects.builtins.functions.Eval_F;
import kevwargo.jlp.objects.builtins.functions.Format_F;
import kevwargo.jlp.objects.builtins.functions.HasNext_F;
import kevwargo.jlp.objects.builtins.functions.IsInstance_F;
import kevwargo.jlp.objects.builtins.functions.Minus_F;
import kevwargo.jlp.objects.builtins.functions.Multiply_F;
import kevwargo.jlp.objects.builtins.functions.Next_F;
import kevwargo.jlp.objects.builtins.functions.Not_F;
import kevwargo.jlp.objects.builtins.functions.Nth_F;
import kevwargo.jlp.objects.builtins.functions.Plus_F;
import kevwargo.jlp.objects.builtins.functions.PrintNamespace_F;
import kevwargo.jlp.objects.builtins.functions.Print_F;
import kevwargo.jlp.objects.builtins.javareflect.AccessField;
import kevwargo.jlp.objects.builtins.javareflect.CallMethod;
import kevwargo.jlp.objects.builtins.javareflect.CreateInstance;
import kevwargo.jlp.objects.builtins.javareflect.LoadClass;
import kevwargo.jlp.objects.builtins.macros.Collect_M;
import kevwargo.jlp.objects.builtins.macros.Defclass_M;
import kevwargo.jlp.objects.builtins.macros.Defmacro_M;
import kevwargo.jlp.objects.builtins.macros.Defun_M;
import kevwargo.jlp.objects.builtins.macros.Dot_M;
import kevwargo.jlp.objects.builtins.macros.If_M;
import kevwargo.jlp.objects.builtins.macros.Lambda_M;
import kevwargo.jlp.objects.builtins.macros.LetStar_M;
import kevwargo.jlp.objects.builtins.macros.Let_M;
import kevwargo.jlp.objects.builtins.macros.Progn_M;
import kevwargo.jlp.objects.builtins.macros.Quote_M;
import kevwargo.jlp.objects.builtins.macros.Setq_M;
import kevwargo.jlp.objects.builtins.macros.loop.For_M;
import kevwargo.jlp.objects.builtins.macros.loop.LispLoopException;
import kevwargo.jlp.objects.builtins.macros.loop.While_M;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.parser.LispParser;
import kevwargo.jlp.utils.LispNamespace;

public class LispProcessor {

    private static LispProcessor instance;
    private LispNamespace namespace;

    private boolean verbose;


    public static LispProcessor getInstance() {
        if (instance == null) {
            instance = new LispProcessor();
        }
        return instance;
    }

    private LispProcessor() {
        initNamespace();
        loadInitFile();
    }

    private void initNamespace() {
        HashMap<String, LispObject> map = new HashMap<String, LispObject>();

        map.put("object", LispType.OBJECT);
        map.put("type", LispType.TYPE);
        map.put("builtin-function", LispType.FUNCTION);
        map.put("builtin-macro", LispType.MACRO);
        map.put("function", LispType.FUNCTION);
        map.put("macro", LispType.MACRO);
        map.put("str", LispType.STRING);
        map.put("bool", LispType.BOOL);
        map.put("int", LispType.INT);
        map.put("float", LispType.FLOAT);
        map.put("list", LispType.LIST);
        map.put("symbol", LispType.SYMBOL);
        map.put("method", LispType.METHOD);
        map.put("java-object", LispType.JAVA_OBJECT);
        map.put("iterator", LispType.ITERATOR);
        
        map.put("print", new Print_F());
        map.put("format", new Format_F());
        map.put("quote", new Quote_M());
        map.put("apply", new Apply_F());
        map.put("concat", new Concat_F());
        map.put("defun", new Defun_M());
        map.put("defmacro", new Defmacro_M());
        map.put("defclass", new Defclass_M());
        map.put("let", new Let_M());
        map.put("let*", new LetStar_M());
        map.put("append", new Append_F());
        map.put("if", new If_M());
        map.put("while", new While_M());
        map.put("for", new For_M());
        map.put("progn", new Progn_M());
        map.put("nth", new Nth_F());
        map.put("lambda", new Lambda_M());
        map.put("setq", new Setq_M());
        map.put("eval", new Eval_F());
        map.put("print-namespace", new PrintNamespace_F());
        map.put(".", new Dot_M());
        map.put("next", new Next_F());
        map.put("has-next", new HasNext_F());

        map.put("+", new Plus_F());
        map.put("-", new Minus_F());
        map.put("*", new Multiply_F());
        map.put("/", new Divide_F());

        map.put("not", new Not_F());
        map.put("eq", new Eq_F());
        map.put("equalp", new Equalp_F());
        map.put("isinstance", new IsInstance_F());
        map.put("collect", new Collect_M());

        map.put("%class", new LoadClass());
        map.put("%new", new CreateInstance());
        map.put("%call", new CallMethod());
        map.put("%get", new AccessField(false));
        map.put("%set", new AccessField(true));

        namespace = new LispNamespace(map);
    }

    private void loadInitFile() {
        InputStream is = getClass().getResourceAsStream("/jlp/init.lisp");
        if (is == null) {
            return;
        }

        LispParser parser = new LispParser(is);
        try {
            process(parser);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void define(String name, LispObject definition) {
        namespace.bind(name, definition);
    }

    public void setVerbose(boolean verbose) {
        this.verbose = verbose;
    }

    public void process(LispParser parser) throws IOException, LispException {
        process(parser, System.out);
    }
    
    public void process(LispParser parser, PrintStream outStream) throws IOException, LispException {
        LispObject lispObject;
        HashMap<String, LispObject> map = new HashMap<String, LispObject>();
        map.put("*out*", new LispJavaObject(outStream));
        LispNamespace namespace = this.namespace.prepend(map);
        while ((lispObject = parser.read()) != null) {
            try {
                LispObject result = lispObject.eval(namespace);
                if (verbose) {
                    System.out.println(result.repr());
                }
            } catch (LispLoopException e) {}
        }
    }

}
