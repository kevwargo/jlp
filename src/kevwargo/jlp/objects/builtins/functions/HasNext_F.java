package kevwargo.jlp.objects.builtins.functions;

import java.util.HashMap;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.iterator.LispIterator;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class HasNext_F extends LispFunction {

    public HasNext_F() {
        super(LispType.FUNCTION, "has-next", new FormalArguments().pos("obj"));
    }

    protected LispObject callInternal(LispNamespace namespace, HashMap<String, LispObject> arguments) throws LispException {
        LispObject obj = arguments.get("obj");
        if (!obj.isInstance(LispType.ITERATOR)) {
            throw new LispException("object '%s' is not an iterator");
        }
        boolean hasNext = ((LispIterator)obj.cast(LispType.ITERATOR)).hasNext();
        return hasNext ? LispBool.T : LispBool.NIL;
    }

}
