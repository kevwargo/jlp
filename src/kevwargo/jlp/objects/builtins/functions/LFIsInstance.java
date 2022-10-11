package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;

public class LFIsInstance extends LispFunction {

    public LFIsInstance() {
        super(LispType.FUNCTION, "isinstance", new CallArgs("obj", "type"));
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        LispObject obj = args.get("obj");
        LispType type = (LispType) args.get("type").cast(LispType.TYPE);
        if (obj.isInstance(type)) {
            return LispBool.TRUE;
        }
        return LispBool.FALSE;
    }
}
