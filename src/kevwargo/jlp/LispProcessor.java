package kevwargo.jlp;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.builtins.functions.LFAppend;
import kevwargo.jlp.objects.builtins.functions.LFApply;
import kevwargo.jlp.objects.builtins.functions.LFBoundp;
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
        namespace = new LispNamespace();
        initNamespace();
        loadInitFile();
    }

    private void initNamespace() {
        define("object", LispType.OBJECT);
        define("type", LispType.TYPE);
        define("builtin-function", LispType.FUNCTION);
        define("builtin-macro", LispType.MACRO);
        define("function", LispType.FUNCTION);
        define("macro", LispType.MACRO);
        define("str", LispType.STRING);
        define("bool", LispType.BOOL);
        define("int", LispType.INT);
        define("float", LispType.FLOAT);
        define("list", LispType.LIST);
        define("symbol", LispType.SYMBOL);
        define("method", LispType.METHOD);
        define("java-object", LispType.JAVA_OBJECT);
        define("iterator", LispType.ITERATOR);

        define(new LMBackquote());
        define(new LFPrint());
        define(new LFFormat());
        define(new LMQuote());
        define(new LFApply());
        define(new LFCapitalize());
        define(new LFConcat());
        define(new LMDefun());
        define(new LMDefmacro());
        define(new LMDefclass());
        define(new LMLet());
        define(new LMLetStar());
        define(new LFAppend());
        define(new LMIf());
        define(new LMFor());
        define(new LMProgn());
        define(new LFNth());
        define(new LMLambda());
        define(new LMSetq());
        define(new LFEval());
        define(new LFPrintNamespace());
        define(new LMDot());
        define(new LFNext());
        define(new LFHasNext());

        define(new LFPlus());
        define(new LFMinus());
        define(new LFMultiply());
        define(new LFDivide());

        define(new LFNot());
        define(new LFEq());
        define(new LFEqualp());
        define(new LFIsInstance());
        define(new LMCollect());
        define(new LFBoundp());
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
