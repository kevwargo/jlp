package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispNil;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;

import java.util.Iterator;

public class LMDefun extends LispFunction {

    public static String NAME = "defun";
    public static String ARG_NAME = "name";
    public static String ARG_ARGLIST = "arglist";
    public static String ARG_BODY = "body";

    private static CallArgs namedArgs = new CallArgs(ARG_NAME, ARG_ARGLIST).rest(ARG_BODY);
    private static CallArgs anonymousArgs = new CallArgs(ARG_ARGLIST).rest(ARG_BODY);
    private boolean isMacro;
    private boolean isAnonymous;

    public LMDefun() {
        this(NAME, false, false);
    }

    protected LMDefun(String name, boolean isMacro, boolean isAnonymous) {
        super(LispType.MACRO, name, isAnonymous ? anonymousArgs : namedArgs);
        this.isMacro = isMacro;
        this.isAnonymous = isAnonymous;
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        String name;
        if (isAnonymous) {
            name = "<lambda>";
        } else {
            name = ((LispSymbol) args.get(ARG_NAME).cast(LispType.SYMBOL)).getName();
        }

        LispList arglist = (LispList) args.get(ARG_ARGLIST).cast(LispType.LIST);
        LispList body = (LispList) args.get(ARG_BODY).cast(LispType.LIST);
        LispNamespace namespace = runtime.getNS();

        LispFunction function = new Function(name, new CallArgs(arglist, runtime), body, namespace);
        namespace.bind(name, function);

        return function;
    }

    private class Function extends LispFunction {

        LispList body;
        LispNamespace defNamespace;

        public Function(String name, CallArgs args, LispList body, LispNamespace namespace) {
            super(LMDefun.this.isMacro ? LispType.LISP_MACRO : LispType.LISP_FUNCTION, name, args);
            this.body = body;
            defNamespace = namespace;
        }

        public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
            LispNamespace.Layer layer = new LispNamespace.Layer();
            layer.define(new ReturnFunction());
            layer.put("$", this);

            runtime = runtime.with(defNamespace).with(args).with(layer);

            Iterator<LispObject> it = body.iterator();
            LispObject result = LispNil.NIL;
            try {
                while (it.hasNext()) {
                    result = it.next().eval(runtime);
                }
            } catch (ReturnException re) {
                return re.object;
            }
            return result;
        }
    }

    private static class ReturnException extends LispException {

        public LispObject object;

        public ReturnException(LispObject object) {
            super("'return' is used outside of a function");
            this.object = object;
        }
    }

    private static class ReturnFunction extends LispFunction {

        private static final String ARG_OBJ = "obj";

        public ReturnFunction() {
            super(LispType.FUNCTION, "return", new CallArgs(ARG_OBJ));
        }

        public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
            throw new ReturnException(args.get(ARG_OBJ));
        }
    }
}
