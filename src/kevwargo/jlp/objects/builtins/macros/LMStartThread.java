package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBaseObject;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

import java.io.PrintStream;

public class LMStartThread extends LispFunction {

    public static final String NAME = "start-thread";
    public static final String ARG_BODY = "body";

    public LMStartThread() {
        super(LispType.MACRO, NAME, new CallArgs().rest(ARG_BODY));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        LispThread thread = new LispThread(runtime, (LispList) args.get(ARG_BODY));
        thread.start();
        return LispBaseObject.wrap(thread);
    }

    private static class LispThread extends Thread {
        private LispRuntime runtime;
        private LispList body;

        public LispThread(LispRuntime runtime, LispList body) {
            this.runtime = runtime;
            this.body = body;
        }

        public void run() {
            try {
                for (LispObject form : body) {
                    form.eval(runtime);
                }
            } catch (LispException exc) {
                exc.printStackTrace(new PrintStream(runtime.getOut()));
            }
        }
    }
}
