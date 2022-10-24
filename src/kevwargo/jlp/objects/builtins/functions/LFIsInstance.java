package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LFIsInstance extends LispFunction {

    public static final String NAME = "isinstance";
    public static final String ARG_OBJ = "obj";
    public static final String ARG_TYPE = "type";

    public LFIsInstance() {
        super(LispType.FUNCTION, NAME, new CallArgs(ARG_OBJ, ARG_TYPE));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        LispObject obj = args.get(ARG_OBJ);
        LispType type = (LispType) args.get(ARG_TYPE).cast(LispType.TYPE);

        return obj.isInstance(type) ? LispBool.TRUE : LispBool.FALSE;
    }
}
