package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispNil;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LFDel extends LispFunction {

    public static String NAME = "del";
    public static String ARG_SYMBOL = "symbol";

    public LFDel() {
        super(LispType.FUNCTION, NAME, new CallArgs(ARG_SYMBOL));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        String name = ((LispSymbol) args.get(ARG_SYMBOL).cast(LispType.SYMBOL)).getName();
        runtime.getNS().delete(name);

        return LispNil.NIL;
    }
}
