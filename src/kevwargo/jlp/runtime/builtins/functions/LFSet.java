package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispSymbol;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LFSet extends LispFunction {

    public static final String NAME = "set";
    public static final String ARG_SYMBOL = "symbol";
    public static final String ARG_DEFINITION = "definition";

    private boolean global;

    public LFSet() {
        this(NAME, false);
    }

    protected LFSet(String name, boolean global) {
        super(LispType.FUNCTION, name, new CallArgs(ARG_SYMBOL, ARG_DEFINITION));
        this.global = global;
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        String name = ((LispSymbol) args.get(ARG_SYMBOL).cast(LispType.SYMBOL)).getName();
        LispObject definition = args.get(ARG_DEFINITION);

        runtime.getNS().bind(name, definition, global);
        return definition;
    }
}
