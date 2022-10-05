package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;

import java.util.Map;

public class LFBoundp extends LispFunction {

    public static String NAME = "boundp";
    public static String ARG_SYMBOL = "symbol";

    public LFBoundp() {
        super(LispType.FUNCTION, NAME, new FormalArguments(ARG_SYMBOL));
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments)
            throws LispException {
        LispSymbol symbol = (LispSymbol) arguments.get(ARG_SYMBOL).cast(LispType.SYMBOL);
        return namespace.get(symbol.getName()) == null ? LispBool.NIL : LispBool.T;
    }
}
