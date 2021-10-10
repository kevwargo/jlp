package kevwargo.jlp.objects.builtins.functions;

import java.util.Map;
import java.util.NoSuchElementException;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.iterator.LispIterator;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class Next_F extends LispFunction {

    public Next_F() {
        super(LispType.FUNCTION, "next", new FormalArguments().pos("obj"));
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
        LispObject obj = arguments.get("obj");
        if (!obj.isInstance(LispType.ITERATOR)) {
            throw new LispException("object '%s' is not an iterator");
        }
        try {
            return ((LispIterator)obj.cast(LispType.ITERATOR)).next();
        } catch (NoSuchElementException e) {
            return LispBool.NIL;
        }
    }

}
