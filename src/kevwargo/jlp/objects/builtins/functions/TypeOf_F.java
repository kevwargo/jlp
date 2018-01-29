package kevwargo.jlp.objects.builtins.functions;

import java.util.HashMap;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class TypeOf_F extends LispFunction {

    public TypeOf_F() {
        super(LispType.FUNCTION, "type-of", new FormalArguments().pos("obj"));
    }

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        return arguments.get("obj").getType();
    }
    
}
