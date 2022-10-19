package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;

public class LFEq extends LispFunction {

    public static final String NAME = "eq";
    public static final String ARG_OBJ1 = "obj1";
    public static final String ARG_OBJ2 = "obj2";

    protected LFEq(String name) {
        super(LispType.FUNCTION, name, new CallArgs(ARG_OBJ1, ARG_OBJ2));
    }

    public LFEq() {
        this(NAME);
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        LispObject obj1 = args.get(ARG_OBJ1);
        LispObject obj2 = args.get(ARG_OBJ2);

        if (obj1 == obj2) {
            return LispBool.TRUE;
        }

        if (obj1.isInstance(LispType.JAVA_OBJECT) && obj2.isInstance(LispType.JAVA_OBJECT)) {
            if (((LispJavaObject) obj1.cast(LispType.JAVA_OBJECT)).getObject()
                    == ((LispJavaObject) obj2.cast(LispType.JAVA_OBJECT)).getObject()) {
                return LispBool.TRUE;
            }
        }

        return LispBool.FALSE;
    }
}
