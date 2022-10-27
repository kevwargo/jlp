package kevwargo.jlp.runtime.builtins.functions.compare;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.scalars.LispBool;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LFLess extends CompareFunction {

    public static final String NAME = "<";

    public LFLess() {
        super(NAME);
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        if (compare(args.get(ARG_OBJ1), args.get(ARG_OBJ2)) < 0) {
            return LispBool.TRUE;
        }
        return LispBool.FALSE;
    }
}
