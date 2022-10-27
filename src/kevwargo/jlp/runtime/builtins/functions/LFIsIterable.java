package kevwargo.jlp.runtime.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.objects.iter.LispIterator;
import kevwargo.jlp.objects.scalars.LispBool;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LFIsIterable extends LispFunction {

    public static final String NAME = "isiterable";
    public static final String ARG_OBJ = "obj";

    public LFIsIterable() {
        super(LispFunction.FUNCTION_TYPE, NAME, new CallArgs(ARG_OBJ));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        return LispBool.valueOf(LispIterator.isIterable(args.get(ARG_OBJ)));
    }
}
