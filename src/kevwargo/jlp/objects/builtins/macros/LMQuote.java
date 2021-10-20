package kevwargo.jlp.objects.builtins.macros;

import java.util.Iterator;
import java.util.Map;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class LMQuote extends LispFunction {

    public LMQuote() {
        super(LispType.MACRO, "quote", new FormalArguments().pos("obj"));
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
        return arguments.get("obj");
    }

}
