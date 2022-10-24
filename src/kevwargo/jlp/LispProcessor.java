package kevwargo.jlp;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispNamedObject;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.objects.builtins.functions.*;
import kevwargo.jlp.objects.builtins.functions.compare.*;
import kevwargo.jlp.objects.builtins.functions.math.*;
import kevwargo.jlp.objects.builtins.macros.*;
import kevwargo.jlp.objects.builtins.macros.loop.*;
import kevwargo.jlp.parser.LispParser;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;
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
    }

    private void defineTypes() {
        define(LispType.BOOL);
        define(LispType.FLOAT);
        define(LispType.FUNCTION);
        define(LispType.NUMBER);
        define(LispType.INT);
        define(LispType.ITERATOR);
        define(LispType.JAVA_OBJECT);
        define(LispType.LIST);
        define(LispType.MACRO);
        define(LispType.METHOD);
        define(LispType.OBJECT);
        define(LispType.STRING);
        define(LispType.SYMBOL);
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
