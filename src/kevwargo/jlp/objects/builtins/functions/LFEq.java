package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.utils.FormalArguments;

import java.util.Map;

public class LFEq extends LispFunction {

    LFEq(String name) {
        super(LispType.FUNCTION, name, new FormalArguments("arg1", "arg2"));
    }

    public LFEq() {
        this("eq");
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments)
            throws LispException {
        LispObject obj1 = arguments.get("arg1");
        LispObject obj2 = arguments.get("arg2");

        if (obj1 == obj2) {
            return LispBool.T;
        }

        if (obj1.isInstance(LispType.JAVA_OBJECT) && obj2.isInstance(LispType.JAVA_OBJECT)) {
            if (((LispJavaObject) obj1.cast(LispType.JAVA_OBJECT)).getObject()
                    == ((LispJavaObject) obj2.cast(LispType.JAVA_OBJECT)).getObject()) {
                return LispBool.T;
            }
        }

        return LispBool.NIL;
    }
}
