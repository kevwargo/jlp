package kevwargo.jlp.runtime.builtins.exceptions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

import java.lang.reflect.InvocationTargetException;

public class LMTry extends LispFunction {

    public static final String NAME = "try";
    public static final String ARG_FORM = "form";
    public static final String ARG_CLAUSES = "clauses";

    public LMTry() {
        super(LispType.MACRO, NAME, new CallArgs(ARG_FORM).rest(ARG_CLAUSES));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        Handlers handlers = new Handlers((LispList) args.get(ARG_CLAUSES), runtime);

        try {
            return args.get(ARG_FORM).eval(runtime);
        } catch (LispException exc) {
            Throwable throwable = exc.getCause();
            if (throwable == null) {
                throwable = exc;
            } else if (throwable instanceof InvocationTargetException) {
                throwable = throwable.getCause();
            }

            Handler handler = handlers.findHandler(throwable);
            if (handler != null) {
                return handler.handle(throwable);
            }
            throw exc;
        } finally {
            handlers.runFinally(runtime);
        }
    }
}
