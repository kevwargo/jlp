package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.calls.CallArgs;

public class LFNth extends LispFunction {

    public LFNth() {
        super(LispType.FUNCTION, "nth", new CallArgs("n", "list"));
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        long idx = ((LispInt) args.get("n").cast(LispType.INT)).getValue();
        LispList list = (LispList) args.get("list").cast(LispType.LIST);

        return list.get((int) idx);
    }
}
