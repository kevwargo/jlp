package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.FormalArguments;

import java.util.Map;

public class LFCapitalize extends LispFunction {

    public static final String NAME = "capitalize";
    public static final String ARG_STRING = "string";

    public LFCapitalize() {
        super(LispType.FUNCTION, NAME, new FormalArguments(ARG_STRING));
    }

    protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        String string = ((LispString) arguments.get(ARG_STRING).cast(LispType.STRING)).getValue();
        return new LispString(string.substring(0, 1).toUpperCase() + string.substring(1));
    }
}
