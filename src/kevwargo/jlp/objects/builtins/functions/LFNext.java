package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispIterator;
import kevwargo.jlp.objects.LispNil;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;

import java.util.NoSuchElementException;

public class LFNext extends LispFunction {

    public LFNext() {
        super(LispType.FUNCTION, "next", new CallArgs("obj"));
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        LispObject obj = args.get("obj");

        try {
            return ((LispIterator) obj.cast(LispType.ITERATOR)).next();
        } catch (NoSuchElementException e) {
            return LispNil.NIL;
        }
    }
}
