package kevwargo.jlp.runtime.builtins.macros;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LMQuote extends LispFunction {

    public LMQuote() {
        super(LispFunction.MACRO_TYPE, "quote", new CallArgs("obj"));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        return args.get("obj");
    }
}
