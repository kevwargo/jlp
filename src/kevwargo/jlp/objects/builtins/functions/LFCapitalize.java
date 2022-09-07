package kevwargo.jlp.objects.builtins.functions;

import java.util.Map;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class LFCapitalize extends LispFunction {

    public static final String NAME = "capitalize";
    public static final String ARG_STRING = "string";

    public LFCapitalize() {
        super(LispType.FUNCTION, NAME, new FormalArguments(ARG_STRING));
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
        String string = ((LispString)arguments.get(ARG_STRING).cast(LispType.STRING)).getValue();
        return new LispString(string.substring(0, 1).toUpperCase() + string.substring(1));
    }

}
