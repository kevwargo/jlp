package kevwargo.jlp.objects;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.ArgumentsIterator;

public class LispNil extends LispBaseObject {

    public static final LispNil NIL = new LispNil();

    private LispNil() {
        super(LispType.NIL);
    }

    public String repr() {
        return "nil";
    }

    public boolean bool() {
        return false;
    }

    public Object getJavaObject() {
        return null;
    }

    public Class<?> getJavaClass() {
        return Object.class;
    }
}

class NilType extends LispType {

    NilType() {
        super("nil", new LispType[] {OBJECT});
    }

    public LispObject makeInstance(LispRuntime runtime, ArgumentsIterator arguments)
            throws LispException {
        throw new LispException("Cannot instantiate nil");
    }
}
