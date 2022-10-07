package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispIterator;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.FormalArguments;

import java.util.Map;

public class LFIsIterable extends LispFunction {

    public static final String NAME = "isiterable";
    public static final String ARG_OBJ = "obj";

    public LFIsIterable() {
        super(LispType.FUNCTION, NAME, new FormalArguments(ARG_OBJ));
    }

    protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        if (LispIterator.isIterable(arguments.get(ARG_OBJ))) {
            return LispBool.TRUE;
        }
        return LispBool.FALSE;
    }
}
