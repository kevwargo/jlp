package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.FormalArguments;

import java.util.Map;

public class LFNot extends LispFunction {

    public static final String NAME = "!";
    public static final String ARG_OBJ = "!";

    public LFNot() {
        super(LispType.FUNCTION, NAME, new FormalArguments(ARG_OBJ));
    }

    protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        return arguments.get(ARG_OBJ).bool() ? LispBool.FALSE : LispBool.TRUE;
    }
}
