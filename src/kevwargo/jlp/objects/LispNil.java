package kevwargo.jlp.objects;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

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

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        throw new LispException("Cannot instantiate nil");
    }
}
