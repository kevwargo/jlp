package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispIterator;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LFHasNext extends LispFunction {

    public LFHasNext() {
        super(LispType.FUNCTION, "has-next", new CallArgs("obj"));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        LispObject obj = args.get("obj");
        boolean hasNext = ((LispIterator) obj.cast(LispType.ITERATOR)).hasNext();

        return hasNext ? LispBool.TRUE : LispBool.FALSE;
    }
}