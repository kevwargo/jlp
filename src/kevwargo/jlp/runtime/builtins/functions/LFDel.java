package kevwargo.jlp.runtime.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.objects.scalars.LispNil;
import kevwargo.jlp.objects.scalars.LispSymbol;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LFDel extends LispFunction {

    public static String NAME = "del";
    public static String ARG_SYMBOL = "symbol";

    public LFDel() {
        super(LispFunction.FUNCTION_TYPE, NAME, new CallArgs(ARG_SYMBOL));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        String name = ((LispSymbol) args.get(ARG_SYMBOL).cast(LispSymbol.TYPE)).getName();
        runtime.getNS().delete(name);

        return LispNil.NIL;
    }
}
