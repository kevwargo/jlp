package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.FormalArguments;

import java.util.Map;

public class LFIsInstance extends LispFunction {

    public LFIsInstance() {
        super(LispType.FUNCTION, "isinstance", new FormalArguments("obj", "type"));
    }

    protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        LispObject obj = arguments.get("obj");
        LispType type = (LispType) arguments.get("type").cast(LispType.TYPE);
        if (obj.isInstance(type)) {
            return LispBool.T;
        }
        return LispBool.NIL;
    }
}
