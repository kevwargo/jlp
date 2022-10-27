package kevwargo.jlp.runtime.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.objects.scalars.LispBool;
import kevwargo.jlp.objects.scalars.LispSymbol;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LFBoundp extends LispFunction {

    public static String NAME = "boundp";
    public static String ARG_SYMBOL = "symbol";

    public LFBoundp() {
        super(LispFunction.FUNCTION_TYPE, NAME, new CallArgs(ARG_SYMBOL));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        LispSymbol symbol = (LispSymbol) args.get(ARG_SYMBOL).cast(LispSymbol.TYPE);
        return LispBool.valueOf(runtime.getNS().get(symbol.getName()) != null);
    }
}
