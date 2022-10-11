package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispCallable;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;

public class LFApply extends LispFunction {

    public static final String NAME = "apply";
    public static final String ARG_CALLABLE = "callable";
    public static final String ARG_ARGS = "args";

    public LFApply() {
        super(LispType.FUNCTION, NAME, new CallArgs(ARG_CALLABLE, ARG_ARGS));
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        LispCallable callable = (LispCallable) args.get(ARG_CALLABLE);
        LispList arglist = (LispList) args.get(ARG_ARGS).cast(LispType.LIST);

        return arglist.applyCallable(callable, runtime);
    }
}
