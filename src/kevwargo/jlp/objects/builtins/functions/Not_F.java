package kevwargo.jlp.objects.builtins.functions;

import java.util.HashMap;
import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;

public class Not_F extends LispFunction {

    public Not_F() {
        super(LispType.FUNCTION, "not", new FormalArguments().pos("obj"));
    }

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        if (arguments.get("obj") == LispBool.FALSE) {
            return LispBool.TRUE;
        }
        return LispBool.FALSE;
    }

}
