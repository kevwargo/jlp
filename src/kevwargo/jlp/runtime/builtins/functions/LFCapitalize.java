package kevwargo.jlp.runtime.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.collections.LispString;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LFCapitalize extends LispFunction {

    public static final String NAME = "capitalize";
    public static final String ARG_STRING = "string";

    public LFCapitalize() {
        super(LispFunction.FUNCTION_TYPE, NAME, new CallArgs(ARG_STRING));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        String string = ((LispString) args.get(ARG_STRING).cast(LispString.TYPE)).getValue();
        return new LispString(string.substring(0, 1).toUpperCase() + string.substring(1));
    }
}
