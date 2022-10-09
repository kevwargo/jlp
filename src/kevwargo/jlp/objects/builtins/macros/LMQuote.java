package kevwargo.jlp.objects.builtins.macros;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.CallArgs;

public class LMQuote extends LispFunction {

    public LMQuote() {
        super(LispType.MACRO, "quote", new CallArgs("obj"));
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        return args.get("obj");
    }
}
