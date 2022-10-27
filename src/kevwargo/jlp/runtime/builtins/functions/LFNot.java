package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LFNot extends LispFunction {

    public static final String NAME = "!";
    public static final String ARG_OBJ = "obj";

    public LFNot() {
        super(LispType.FUNCTION, NAME, new CallArgs(ARG_OBJ));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        return args.get(ARG_OBJ).bool() ? LispBool.FALSE : LispBool.TRUE;
    }
}