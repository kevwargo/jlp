package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispIterator;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.calls.CallArgs;

public class LFIsIterable extends LispFunction {

    public static final String NAME = "isiterable";
    public static final String ARG_OBJ = "obj";

    public LFIsIterable() {
        super(LispType.FUNCTION, NAME, new CallArgs(ARG_OBJ));
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        if (LispIterator.isIterable(args.get(ARG_OBJ))) {
            return LispBool.TRUE;
        }
        return LispBool.FALSE;
    }
}
