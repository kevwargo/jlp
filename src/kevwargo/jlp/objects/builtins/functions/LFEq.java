package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.CallArgs;

public class LFEq extends LispFunction {

    LFEq(String name) {
        super(LispType.FUNCTION, name, new CallArgs("arg1", "arg2"));
    }

    public LFEq() {
        this("eq");
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        LispObject obj1 = args.get("arg1");
        LispObject obj2 = args.get("arg2");

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
