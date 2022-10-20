package kevwargo.jlp.objects.builtins.functions.compare;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LFEq extends CompareFunction {

    public static final String NAME = "==";

    public LFEq() {
        super(NAME);
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        if (equalp(args.get(ARG_OBJ1), args.get(ARG_OBJ2), false)) {
            return LispBool.TRUE;
        }
        return LispBool.FALSE;
    }
}
