package kevwargo.jlp;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispBaseObject;
import kevwargo.jlp.objects.base.LispNamedObject;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.base.LispType;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.objects.functions.LispMethod;
import kevwargo.jlp.objects.iter.LispIterator;
import kevwargo.jlp.objects.scalars.LispBool;
import kevwargo.jlp.objects.scalars.LispString;
import kevwargo.jlp.objects.scalars.LispSymbol;
import kevwargo.jlp.objects.scalars.numbers.LispFloat;
import kevwargo.jlp.objects.scalars.numbers.LispInt;
import kevwargo.jlp.objects.scalars.numbers.LispNumber;
import kevwargo.jlp.objects.wrappers.LispJavaObject;
import kevwargo.jlp.parser.LispParser;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.runtime.builtins.exceptions.LMTry;
import kevwargo.jlp.runtime.builtins.functions.LFAppend;
import kevwargo.jlp.runtime.builtins.functions.LFApply;
import kevwargo.jlp.runtime.builtins.functions.LFBoundp;
import kevwargo.jlp.runtime.builtins.functions.LFCapitalize;
import kevwargo.jlp.runtime.builtins.functions.LFConcat;
import kevwargo.jlp.runtime.builtins.functions.LFDel;
import kevwargo.jlp.runtime.builtins.functions.LFEval;
import kevwargo.jlp.runtime.builtins.functions.LFFormat;
import kevwargo.jlp.runtime.builtins.functions.LFHasNext;
import kevwargo.jlp.runtime.builtins.functions.LFIsInstance;
import kevwargo.jlp.runtime.builtins.functions.LFIsIterable;
import kevwargo.jlp.runtime.builtins.functions.LFNext;
import kevwargo.jlp.runtime.builtins.functions.LFNot;
import kevwargo.jlp.runtime.builtins.functions.LFNth;
import kevwargo.jlp.runtime.builtins.functions.LFPrint;
import kevwargo.jlp.runtime.builtins.functions.LFPrintNamespace;
import kevwargo.jlp.runtime.builtins.functions.LFSet;
import kevwargo.jlp.runtime.builtins.functions.LFSetGlobal;
import kevwargo.jlp.runtime.builtins.functions.compare.LFEq;
import kevwargo.jlp.runtime.builtins.functions.compare.LFEqStrict;
import kevwargo.jlp.runtime.builtins.functions.compare.LFGreater;
import kevwargo.jlp.runtime.builtins.functions.compare.LFLess;
import kevwargo.jlp.runtime.builtins.functions.math.LFDivide;
import kevwargo.jlp.runtime.builtins.functions.math.LFMinus;
import kevwargo.jlp.runtime.builtins.functions.math.LFMultiply;
import kevwargo.jlp.runtime.builtins.functions.math.LFPlus;
import kevwargo.jlp.runtime.builtins.macros.LMBackquote;
import kevwargo.jlp.runtime.builtins.macros.LMChain;
import kevwargo.jlp.runtime.builtins.macros.LMDefclass;
import kevwargo.jlp.runtime.builtins.macros.LMDefmacro;
import kevwargo.jlp.runtime.builtins.macros.LMDefun;
import kevwargo.jlp.runtime.builtins.macros.LMIf;
import kevwargo.jlp.runtime.builtins.macros.LMLambda;
import kevwargo.jlp.runtime.builtins.macros.LMLet;
import kevwargo.jlp.runtime.builtins.macros.LMLetStar;
import kevwargo.jlp.runtime.builtins.macros.LMProgn;
import kevwargo.jlp.runtime.builtins.macros.LMQuote;
import kevwargo.jlp.runtime.builtins.macros.LMStartThread;
import kevwargo.jlp.runtime.builtins.macros.loop.LMFor;
import kevwargo.jlp.runtime.builtins.range.LTRange;

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
    private Layer builtins;

    public static LispProcessor getInstance() {
        if (instance == null) {
            instance = new LispProcessor();
        }
        return instance;
    }

    private LispProcessor() {
        builtins = new Layer(false);
        namespace = new LispNamespace(builtins);

        defineBuiltins();
        loadInitFile();
    }

    private void defineBuiltins() {
        defineFunctions();
        defineMacros();
        defineTypes();
    }

    private void defineFunctions() {
        define(new LFAppend());
        define(new LFApply());
        define(new LFBoundp());
        define(new LFCapitalize());
        define(new LFConcat());
        define(new LFDel());
        define(new LFDivide());
        define(new LFEqStrict());
        define(new LFEq());
        define(new LFLess());
        define(new LFGreater());
        define(new LFEval());
        define(new LFFormat());
        define(new LFHasNext());
        define(new LFIsInstance());
        define(new LFIsIterable());
        define(new LFMinus());
        define(new LFMultiply());
        define(new LFNext());
        define(new LFNot());
        define(new LFNth());
        define(new LFPlus());
        define(new LFPrint());
        define(new LFPrintNamespace());
        define(new LFSet());
        define(new LFSetGlobal());
    }

    private void defineMacros() {
        define(new LMBackquote());
        define(new LMDefclass());
        define(new LMDefmacro());
        define(new LMDefun());
        define(new LMChain());
        define(new LMFor());
        define(new LMIf());
        define(new LMLambda());
        define(new LMLet());
        define(new LMLetStar());
        define(new LMProgn());
        define(new LMQuote());
        define(new LMStartThread());
        define(new LMTry());
    }

    private void defineTypes() {
        define(LispBool.TYPE);
        define(LispFloat.TYPE);
        define(LispFunction.FUNCTION_TYPE);
        define(LispNumber.TYPE);
        define(LispInt.TYPE);
        define(LispIterator.TYPE);
        define(LispJavaObject.TYPE);
        define(LispList.TYPE);
        define(LispFunction.MACRO_TYPE);
        define(LispMethod.TYPE);
        define(LispBaseObject.TYPE);
        define(LispString.TYPE);
        define(LispSymbol.TYPE);
        define(LispType.TYPE);
        define(new LTRange());
    }

    private void loadInitFile() {
        InputStream in = getClass().getResourceAsStream("/kevwargo/jlp/init.lisp");
        if (in == null) {
            return;
        }

        try {
            run(in);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void define(LispNamedObject obj) {
        builtins.put(obj.getName(), obj);
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
            final Socket clientSocket = server.accept();

            new Thread() {
                public void run() {
                    try {
                        System.out.printf("JLP client %s connected\n", clientSocket);

                        InputStream in = clientSocket.getInputStream();
                        OutputStream out = clientSocket.getOutputStream();
                        runInteractive(in, out, out);

                        System.out.printf("JLP client %s disconnected\n", clientSocket);
                    } catch (IOException e) {
                        e.printStackTrace();
                    }

                    try {
                        clientSocket.close();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
            }.start();
        }
        server.close();
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
                    printOut.println(result.repr());
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
