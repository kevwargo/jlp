package kevwargo.jlp.objects.builtins.functions;

import java.util.HashMap;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class Eval_F extends LispFunction {

    public Eval_F() {
        super(LispType.FUNCTION, "eval", (new FormalArguments()).pos("object"));
    }

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        return arguments.get("object").eval(namespace);
    }
    
}
