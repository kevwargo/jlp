package kevwargo.jlp.objects;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.ArgumentsIterator;

public class LispBool extends LispBaseObject {

    public static final LispBool TRUE = new LispBool(true);
    public static final LispBool FALSE = new LispBool(false);

    private boolean value;

    private LispBool(boolean value) {
        super(LispType.BOOL);
        this.value = value;
    }

    public boolean getValue() {
        return value;
    }

    public String repr() {
        return value ? "t" : "false";
    }

    public boolean bool() {
        return value;
    }

    public Object getJavaObject() {
        return value;
    }

    public Class<?> getJavaClass() {
        return Boolean.TYPE;
    }
}

class BoolType extends LispType {

    BoolType() {
        super("bool", new LispType[] {OBJECT});
    }

    public LispObject makeInstance(LispRuntime runtime, ArgumentsIterator arguments)
            throws LispException {
        if (!arguments.hasNext()) {
            return LispBool.FALSE;
        }

        return arguments.next().bool() ? LispBool.TRUE : LispBool.FALSE;
    }
}
