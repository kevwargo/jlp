package kevwargo.jlp.runtime.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.objects.iter.LispIterator;
import kevwargo.jlp.objects.scalars.LispBool;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LFHasNext extends LispFunction {

    public LFHasNext() {
        super(LispFunction.FUNCTION_TYPE, "has-next", new CallArgs("obj"));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        LispObject obj = args.get("obj");
        boolean hasNext = ((LispIterator) obj.cast(LispIterator.TYPE)).hasNext();

        return LispBool.valueOf(hasNext);
    }
}
