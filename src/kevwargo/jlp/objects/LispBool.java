package kevwargo.jlp.objects;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.LispNamespace;


public class LispBool extends LispObject {

    public static final LispBool T = new LispBool(true);
    public static final LispBool NIL = new LispBool(false);


    private boolean value;


    private LispBool(boolean value) {
        super(LispType.BOOL);
        this.value = value;
    }

    public boolean getValue() {
        return value;
    }

    public String repr() {
        return value ? "t" : "nil";
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
        super("bool", new LispType[] { OBJECT });
    }

    public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        if (arguments.getLength() < 1) {
            return LispBool.NIL;
        }
        return arguments.next() == LispBool.NIL ? LispBool.NIL : LispBool.T;
    }

}
