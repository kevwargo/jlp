package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.FormalArguments;

import java.util.Map;

public class LFSet extends LispFunction {

    public static final String NAME = "set";
    public static final String ARG_SYMBOL = "symbol";
    public static final String ARG_DEFINITION = "definition";

    private boolean global;

    public LFSet() {
        this(NAME, false);
    }

    protected LFSet(String name, boolean global) {
        super(LispType.FUNCTION, name, new FormalArguments(ARG_SYMBOL, ARG_DEFINITION));
        this.global = global;
    }

    protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        String name = ((LispSymbol) arguments.get(ARG_SYMBOL).cast(LispType.SYMBOL)).getName();
        LispObject definition = arguments.get(ARG_DEFINITION);

        runtime.getNS().bind(name, definition, global);
        return definition;
    }
}
