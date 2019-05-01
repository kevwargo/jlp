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

public class Eq_F extends LispFunction {

    Eq_F(String name) {
        super(LispType.FUNCTION, name, new FormalArguments().pos("arg1").pos("arg2"));
    }

    public Eq_F() {
        this("eq");
    }

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        LispObject obj1 = arguments.get("arg1");
        LispObject obj2 = arguments.get("arg2");
        if (obj1 == obj2) {
            return LispBool.TRUE;
        }
        return LispBool.FALSE;
    }
}
