package kevwargo.jlp.runtime.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.collections.LispString;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LFFormat extends LispFunction {

    public static final String NAME = "format";
    public static final String ARG_FMT = "fmt";
    public static final String ARG_ARGS = "args";

    public LFFormat() {
        super(LispFunction.FUNCTION_TYPE, NAME, new CallArgs(ARG_FMT).rest(ARG_ARGS));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        String fmt = ((LispString) args.get(ARG_FMT).cast(LispString.TYPE)).getValue();

        LispList arglist = (LispList) args.get(ARG_ARGS);
        Object fmtArgs[] = new Object[arglist.size()];
        for (int i = 0; i < fmtArgs.length; i++) {
            fmtArgs[i] = arglist.get(0).format();
        }

        return new LispString(String.format(fmt, fmtArgs));
    }
}
