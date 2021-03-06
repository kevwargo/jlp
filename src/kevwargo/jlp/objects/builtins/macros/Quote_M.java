package kevwargo.jlp.objects.builtins.macros;

import java.util.HashMap;
import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class Quote_M extends LispFunction {

    public Quote_M() {
        super(LispType.MACRO, "quote", new FormalArguments().pos("obj"));
    }

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        return arguments.get("obj");
    }
    
}
