package kevwargo.jlp.runtime.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LFEval extends LispFunction {

    public LFEval() {
        super(LispFunction.FUNCTION_TYPE, "eval", new CallArgs("object"));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        return args.get("object").eval(runtime);
    }
}
