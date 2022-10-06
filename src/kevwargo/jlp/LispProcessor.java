package kevwargo.jlp;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.objects.builtins.functions.LFAppend;
import kevwargo.jlp.objects.builtins.functions.LFApply;
import kevwargo.jlp.objects.builtins.functions.LFBoundp;
import kevwargo.jlp.objects.builtins.functions.LFCapitalize;
import kevwargo.jlp.objects.builtins.functions.LFConcat;
import kevwargo.jlp.objects.builtins.functions.LFDel;
import kevwargo.jlp.objects.builtins.functions.LFEq;
import kevwargo.jlp.objects.builtins.functions.LFEqualp;
import kevwargo.jlp.objects.builtins.functions.LFEval;
import kevwargo.jlp.objects.builtins.functions.LFFormat;
import kevwargo.jlp.objects.builtins.functions.LFHasNext;
import kevwargo.jlp.objects.builtins.functions.LFIsInstance;
import kevwargo.jlp.objects.builtins.functions.LFNext;
import kevwargo.jlp.objects.builtins.functions.LFNot;
import kevwargo.jlp.objects.builtins.functions.LFNth;
import kevwargo.jlp.objects.builtins.functions.LFPrint;
import kevwargo.jlp.objects.builtins.functions.LFPrintNamespace;
import kevwargo.jlp.objects.builtins.functions.LFSet;
import kevwargo.jlp.objects.builtins.functions.LFSetGlobal;
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
import kevwargo.jlp.objects.builtins.macros.LMLet;
import kevwargo.jlp.objects.builtins.macros.LMLetStar;
import kevwargo.jlp.objects.builtins.macros.LMProgn;
import kevwargo.jlp.objects.builtins.macros.LMQuote;
import kevwargo.jlp.objects.builtins.macros.loop.LMFor;
import kevwargo.jlp.parser.LispParser;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.UnknownHostException;

public class LispProcessor {

    private static LispProcessor instance;
    private LispNamespace namespace;
    private LispNamespace.Layer builtins;

    public static LispProcessor getInstance() {
        if (instance == null) {
            instance = new LispProcessor();
        }
        return instance;
    }

    private LispProcessor() {
        builtins = new LispNamespace.Layer(false);
        namespace = new LispNamespace(builtins);

        initBuiltins();
        loadInitFile();
    }

    private void initBuiltins() {
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
        define(new LFSet());
        define(new LFSetGlobal());
        define(new LFDel());
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
        InputStream in = getClass().getResourceAsStream("/jlp/init.lisp");
        if (in == null) {
            return;
        }

        try {
            run(in);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void define(String name, LispObject definition) {
        builtins.put(name, definition);
    }

    public void define(LispFunction function) {
        define(function.getName(), function);
    }

    public void run(InputStream in) throws LispException {
        run(in, System.out, System.err, false);
    }

    public void runInteractive(InputStream in, OutputStream out, OutputStream err) {
        try {
            run(in, out, err, true);
        } catch (LispException exc) {
            // For interactive execute, the exception is handled in `execute()`
        }
    }

    public void runServer(String host, int port) throws UnknownHostException, IOException {
        InetAddress addr = InetAddress.getByName(host);
        ServerSocket server = new ServerSocket(port, 10, addr);

        while (!server.isClosed()) {
            Socket clientSocket = server.accept();
            InputStream in = clientSocket.getInputStream();
            OutputStream out = clientSocket.getOutputStream();
            System.out.printf("JLP client %s connected\n", clientSocket);

            runInteractive(in, out, out);

            clientSocket.close();
            System.out.printf("JLP client %s disconnected\n", clientSocket);
        }
    }

    private void run(InputStream in, OutputStream out, OutputStream err, boolean interactive)
            throws LispException {
        LispRuntime runtime = new LispRuntime(namespace, in, out);
        LispParser parser = new LispParser(in);
        PrintStream printOut = new PrintStream(out);
        PrintStream printErr = new PrintStream(err);

        while (true) {
            if (interactive) {
                printOut.print("JLP> ");
                printOut.flush();
            }

            try {
                LispObject expr = parser.read();
                if (expr == null) {
                    break;
                }

                LispObject result = expr.eval(runtime);
                if (interactive) {
                    printOut.println(result);
                }
            } catch (LispException exc) {
                if (!interactive) {
                    throw exc;
                }
                exc.printStackTrace(printErr);
            }
        }
    }
}
