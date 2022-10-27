package kevwargo.jlp.runtime.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.functions.LispCallable;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LFApply extends LispFunction {

    public static final String NAME = "apply";
    public static final String ARG_CALLABLE = "callable";
    public static final String ARG_ARGS = "args";

    public LFApply() {
        super(LispFunction.FUNCTION_TYPE, NAME, new CallArgs(ARG_CALLABLE, ARG_ARGS));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        LispCallable callable = (LispCallable) args.get(ARG_CALLABLE);
        LispList arglist = (LispList) args.get(ARG_ARGS).cast(LispList.TYPE);

        return arglist.applyCallable(callable, runtime);
    }
}
