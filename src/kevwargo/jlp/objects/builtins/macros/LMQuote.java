package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.FormalArguments;

import java.util.Map;

public class LMQuote extends LispFunction {

    public LMQuote() {
        super(LispType.MACRO, "quote", new FormalArguments("obj"));
    }

    protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        return arguments.get("obj");
    }
}
