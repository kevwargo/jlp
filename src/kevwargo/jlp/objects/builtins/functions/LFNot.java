package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;

import java.util.Map;

public class LFNot extends LispFunction {

    public LFNot() {
        super(LispType.FUNCTION, "not", new FormalArguments("obj"));
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments)
            throws LispException {
        if (arguments.get("obj") == LispBool.NIL) {
            return LispBool.T;
        }
        return LispBool.NIL;
    }
}
