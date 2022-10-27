package kevwargo.jlp.runtime.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.objects.scalars.numbers.LispInt;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LFNth extends LispFunction {

    public LFNth() {
        super(LispFunction.FUNCTION_TYPE, "nth", new CallArgs("n", "list"));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        long idx = ((LispInt) args.get("n").cast(LispInt.TYPE)).getValue();
        LispList list = (LispList) args.get("list").cast(LispList.TYPE);

        return list.get((int) idx);
    }
}
