package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.CallArgs;

public class LFCapitalize extends LispFunction {

    public static final String NAME = "capitalize";
    public static final String ARG_STRING = "string";

    public LFCapitalize() {
        super(LispType.FUNCTION, NAME, new CallArgs(ARG_STRING));
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        String string = ((LispString) args.get(ARG_STRING).cast(LispType.STRING)).getValue();
        return new LispString(string.substring(0, 1).toUpperCase() + string.substring(1));
    }
}
