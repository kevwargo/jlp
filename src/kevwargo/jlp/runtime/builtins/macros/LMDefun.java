package kevwargo.jlp.runtime.builtins.macros;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.base.LispType;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.objects.scalars.LispNil;
import kevwargo.jlp.objects.scalars.LispSymbol;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

import java.util.Iterator;

public class LMDefun extends LispFunction {

    public static String NAME = "defun";
    public static String ARG_NAME = "name";
    public static String ARG_ARGLIST = "arglist";
    public static String ARG_BODY = "body";

    private static CallArgs namedArgs = new CallArgs(ARG_NAME, ARG_ARGLIST).rest(ARG_BODY);
    private static CallArgs anonymousArgs = new CallArgs(ARG_ARGLIST).rest(ARG_BODY);
    private boolean isAnonymous;

    public LMDefun() {
        this(NAME, false);
    }

    protected LMDefun(String name, boolean isAnonymous) {
        super(LispFunction.MACRO_TYPE, name, isAnonymous ? anonymousArgs : namedArgs);
        this.isAnonymous = isAnonymous;
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        String name;
        if (isAnonymous) {
            name = "<lambda>";
        } else {
            name = ((LispSymbol) args.get(ARG_NAME).cast(LispSymbol.TYPE)).getName();
        }

        LispList arglist = (LispList) args.get(ARG_ARGLIST).cast(LispList.TYPE);
        LispList body = (LispList) args.get(ARG_BODY).cast(LispList.TYPE);
        LispNamespace namespace = runtime.getNS();

        LispFunction function = new Function(name, new CallArgs(arglist, runtime), body, namespace);
        namespace.bind(name, function);

        return function;
    }

    protected LispType getFunctionType() {
        return LispFunction.LISP_FUNCTION_TYPE;
    }

    private class Function extends LispFunction {

        LispList body;
        LispNamespace defNamespace;

        public Function(String name, CallArgs args, LispList body, LispNamespace namespace) {
            super(getFunctionType(), name, args);
            this.body = body;
            defNamespace = namespace;
        }

        public LispObject call(LispRuntime runtime, Layer args) throws LispException {
            Layer layer = new Layer();
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
            super(LispFunction.FUNCTION_TYPE, "return", new CallArgs(ARG_OBJ));
        }

        public LispObject call(LispRuntime runtime, Layer args) throws LispException {
            throw new ReturnException(args.get(ARG_OBJ));
        }
    }
}
