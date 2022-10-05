package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispIterator;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
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
        if (!obj.isInstance(LispType.ITERATOR)) {
            throw new LispException("object '%s' is not an iterator");
        }
        try {
            return ((LispIterator) obj.cast(LispType.ITERATOR)).next();
        } catch (NoSuchElementException e) {
            return LispBool.NIL;
        }
    }
}
