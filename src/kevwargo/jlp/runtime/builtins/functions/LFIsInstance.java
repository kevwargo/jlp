package kevwargo.jlp.runtime.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.base.LispType;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.objects.scalars.LispBool;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LFIsInstance extends LispFunction {

    public static final String NAME = "isinstance";
    public static final String ARG_OBJ = "obj";
    public static final String ARG_TYPE = "type";

    public LFIsInstance() {
        super(LispFunction.FUNCTION_TYPE, NAME, new CallArgs(ARG_OBJ, ARG_TYPE));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        LispObject obj = args.get(ARG_OBJ);
        LispType type = (LispType) args.get(ARG_TYPE).cast(LispType.TYPE);

        return LispBool.valueOf(obj.isInstance(type));
    }
}
