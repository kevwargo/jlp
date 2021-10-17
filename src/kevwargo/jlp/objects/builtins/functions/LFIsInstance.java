package kevwargo.jlp.objects.builtins.functions;

import java.util.Map;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class LFIsInstance extends LispFunction {

    public LFIsInstance() {
        super(LispType.FUNCTION, "isinstance", new FormalArguments().pos("obj").pos("type"));
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
        LispObject obj = arguments.get("obj");
        LispType type = (LispType)arguments.get("type").cast(LispType.TYPE);
        if (obj.isInstance(type)) {
            return LispBool.T;
        }
        return LispBool.NIL;
    }

}
