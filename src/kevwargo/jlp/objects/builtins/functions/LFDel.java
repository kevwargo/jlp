package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispNil;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.FormalArguments;

import java.util.Map;

public class LFDel extends LispFunction {

    public static String NAME = "del";
    public static String ARG_SYMBOL = "symbol";

    public LFDel() {
        super(LispType.FUNCTION, NAME, new FormalArguments(ARG_SYMBOL));
    }

    protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        String name = ((LispSymbol) arguments.get(ARG_SYMBOL).cast(LispType.SYMBOL)).getName();
        runtime.getNS().delete(name);

        return LispNil.NIL;
    }
}
