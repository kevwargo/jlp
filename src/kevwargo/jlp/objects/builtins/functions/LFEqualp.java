package kevwargo.jlp.objects.builtins.functions;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFloat;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;

import java.util.Iterator;

public class LFEqualp extends LFEq {

    public static final String NAME = "equalp";

    public LFEqualp() {
        super(NAME);
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        if (super.call(runtime, args) == LispBool.TRUE) {
            return LispBool.TRUE;
        }
        if (equalp(args.get(ARG_OBJ1), args.get(ARG_OBJ2))) {
            return LispBool.TRUE;
        }
        return LispBool.FALSE;
    }

    private boolean equalp(LispObject obj1, LispObject obj2) throws LispException {
        if (obj1.isInstance(LispType.FLOAT) && obj2.isInstance(LispType.FLOAT)) {
            if (((LispFloat) obj1.cast(LispType.FLOAT)).getValue()
                    == ((LispFloat) obj2.cast(LispType.FLOAT)).getValue()) {
                return true;
            }
        }
        if (obj1.isInstance(LispType.FLOAT) && obj2.isInstance(LispType.INT)) {
            if (((LispFloat) obj1.cast(LispType.FLOAT)).getValue()
                    == ((LispInt) obj2.cast(LispType.INT)).getValue()) {
                return true;
            }
        }
        if (obj1.isInstance(LispType.INT) && obj2.isInstance(LispType.FLOAT)) {
            if (((LispInt) obj1.cast(LispType.INT)).getValue()
                    == ((LispFloat) obj2.cast(LispType.FLOAT)).getValue()) {
                return true;
            }
        }
        if (obj1.isInstance(LispType.INT) && obj2.isInstance(LispType.INT)) {
            if (((LispInt) obj1.cast(LispType.INT)).getValue()
                    == ((LispInt) obj2.cast(LispType.INT)).getValue()) {
                return true;
            }
        }
        if (obj1.isInstance(LispType.STRING) && obj2.isInstance(LispType.STRING)) {
            if (((LispString) obj1.cast(LispType.STRING))
                    .getValue()
                    .equals(((LispString) obj2.cast(LispType.STRING)).getValue())) {
                return true;
            }
        }
        if (obj1.isInstance(LispType.LIST) && obj2.isInstance(LispType.LIST)) {
            Iterator<LispObject> it1 = ((LispList) obj1.cast(LispType.LIST)).iterator();
            Iterator<LispObject> it2 = ((LispList) obj2.cast(LispType.LIST)).iterator();
            while (it1.hasNext() && it2.hasNext()) {
                if (!equalp(it1.next(), it2.next())) {
                    return false;
                }
            }
            if (!it1.hasNext() && !it2.hasNext()) {
                return true;
            }
        }
        if (obj1.isInstance(LispType.JAVA_OBJECT) && obj2.isInstance(LispType.JAVA_OBJECT)) {
            if (((LispJavaObject) obj1.cast(LispType.JAVA_OBJECT))
                    .getObject()
                    .equals(((LispJavaObject) obj2.cast(LispType.JAVA_OBJECT)).getObject())) {
                return true;
            }
        }
        return false;
    }
}
