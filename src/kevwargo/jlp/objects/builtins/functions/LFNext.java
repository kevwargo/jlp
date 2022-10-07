package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispIterator;
import kevwargo.jlp.objects.LispNil;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.FormalArguments;

import java.util.Map;
import java.util.NoSuchElementException;

public class LFNext extends LispFunction {

    public LFNext() {
        super(LispType.FUNCTION, "next", new FormalArguments("obj"));
    }

    protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> arguments)
            throws LispException {
        LispObject obj = arguments.get("obj");

        try {
            return ((LispIterator) obj.cast(LispType.ITERATOR)).next();
        } catch (NoSuchElementException e) {
            return LispNil.NIL;
        }
    }
}
