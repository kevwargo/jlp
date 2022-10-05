package kevwargo.jlp.objects.builtins.functions;

import java.util.Map;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispIterator;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class LFHasNext extends LispFunction {

    public LFHasNext() {
        super(LispType.FUNCTION, "has-next", new FormalArguments("obj"));
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
        LispObject obj = arguments.get("obj");
        if (!obj.isInstance(LispType.ITERATOR)) {
            throw new LispException("object '%s' is not an iterator");
        }
        boolean hasNext = ((LispIterator)obj.cast(LispType.ITERATOR)).hasNext();
        return hasNext ? LispBool.T : LispBool.NIL;
    }

}
