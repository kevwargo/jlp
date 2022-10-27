package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LFEval extends LispFunction {

    public LFEval() {
        super(LispType.FUNCTION, "eval", new CallArgs("object"));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        return args.get("object").eval(runtime);
    }
}
