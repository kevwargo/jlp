package kevwargo.jlp.objects.scalars;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispBaseObject;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.base.LispType;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

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

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        if (!args.containsKey(ARG_OBJ)) {
            return LispBool.FALSE;
        }

        return args.get(ARG_OBJ).bool() ? LispBool.TRUE : LispBool.FALSE;
    }
}
