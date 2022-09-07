package kevwargo.jlp;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.builtins.functions.LFAppend;
import kevwargo.jlp.objects.builtins.functions.LFApply;
import kevwargo.jlp.objects.builtins.functions.LFCapitalize;
import kevwargo.jlp.objects.builtins.functions.LFConcat;
import kevwargo.jlp.objects.builtins.functions.LFEq;
import kevwargo.jlp.objects.builtins.functions.LFEqualp;
import kevwargo.jlp.objects.builtins.functions.LFEval;
import kevwargo.jlp.objects.builtins.functions.LFFormat;
import kevwargo.jlp.objects.builtins.functions.LFHasNext;
import kevwargo.jlp.objects.builtins.functions.LFIsInstance;
import kevwargo.jlp.objects.builtins.functions.LFNext;
import kevwargo.jlp.objects.builtins.functions.LFNot;
import kevwargo.jlp.objects.builtins.functions.LFNth;
import kevwargo.jlp.objects.builtins.functions.LFPrintNamespace;
import kevwargo.jlp.objects.builtins.functions.LFPrint;
import kevwargo.jlp.objects.builtins.functions.math.LFDivide;
import kevwargo.jlp.objects.builtins.functions.math.LFMinus;
import kevwargo.jlp.objects.builtins.functions.math.LFMultiply;
import kevwargo.jlp.objects.builtins.functions.math.LFPlus;
import kevwargo.jlp.objects.builtins.macros.LMBackquote;
import kevwargo.jlp.objects.builtins.macros.LMCollect;
import kevwargo.jlp.objects.builtins.macros.LMDefclass;
import kevwargo.jlp.objects.builtins.macros.LMDefmacro;
import kevwargo.jlp.objects.builtins.macros.LMDefun;
import kevwargo.jlp.objects.builtins.macros.LMDot;
import kevwargo.jlp.objects.builtins.macros.LMIf;
import kevwargo.jlp.objects.builtins.macros.LMLambda;
import kevwargo.jlp.objects.builtins.macros.LMLetStar;
import kevwargo.jlp.objects.builtins.macros.LMLet;
import kevwargo.jlp.objects.builtins.macros.LMProgn;
import kevwargo.jlp.objects.builtins.macros.LMQuote;
import kevwargo.jlp.objects.builtins.macros.LMSetq;
import kevwargo.jlp.objects.builtins.macros.loop.LMFor;
import kevwargo.jlp.objects.builtins.macros.loop.LispLoopException;
import kevwargo.jlp.objects.LispType;
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
        Map<String, LispObject> map = new HashMap<String, LispObject>();

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

        map.put("`", new LMBackquote());
        map.put("print", new LFPrint());
        map.put("format", new LFFormat());
        map.put("quote", new LMQuote());
        map.put("apply", new LFApply());
        map.put(LFCapitalize.NAME, new LFCapitalize());
        map.put("concat", new LFConcat());
        map.put("defun", new LMDefun());
        map.put("defmacro", new LMDefmacro());
        map.put("defclass", new LMDefclass());
        map.put("let", new LMLet());
        map.put("let*", new LMLetStar());
        map.put("append", new LFAppend());
        map.put("if", new LMIf());
        map.put("for", new LMFor());
        map.put("progn", new LMProgn());
        map.put("nth", new LFNth());
        map.put("lambda", new LMLambda());
        map.put("setq", new LMSetq());
        map.put("eval", new LFEval());
        map.put("print-namespace", new LFPrintNamespace());
        map.put(".", new LMDot());
        map.put("next", new LFNext());
        map.put("has-next", new LFHasNext());

        map.put("+", new LFPlus());
        map.put("-", new LFMinus());
        map.put("*", new LFMultiply());
        map.put("/", new LFDivide());

        map.put("not", new LFNot());
        map.put("eq", new LFEq());
        map.put("equalp", new LFEqualp());
        map.put("isinstance", new LFIsInstance());
        map.put("collect", new LMCollect());

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

    public void define(LispFunction function) {
        define(function.getName(), function);
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
