package kevwargo.jlp.runtime.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.objects.iter.LispIterator;
import kevwargo.jlp.objects.scalars.LispNil;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

import java.util.NoSuchElementException;

public class LFNext extends LispFunction {

    public LFNext() {
        super(LispFunction.FUNCTION_TYPE, "next", new CallArgs("obj"));
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        LispObject obj = args.get("obj");

        try {
            return ((LispIterator) obj.cast(LispIterator.TYPE)).next();
        } catch (NoSuchElementException e) {
            return LispNil.NIL;
        }
    }
}
