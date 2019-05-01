package kevwargo.jlp.objects.builtins.javareflect;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFloat;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;


public abstract class Caller extends LispFunction {

    public Caller(LispType type, String name, FormalArguments formalArguments) {
        super(type, name, formalArguments);
    }

    protected Object[] extractObjectArgs(LispObject restArgs) throws LispException {
        LispList argList = (LispList)restArgs.cast(LispType.LIST);
        Object args[] = new Object[argList.size()];
        Iterator<LispObject> it = argList.iterator();
        int i = 0;
        while (it.hasNext()) {
            LispObject arg = it.next();
            if (arg.isInstance(LispType.JAVA_OBJECT)) {
                args[i++] = ((LispJavaObject)arg.cast(LispType.JAVA_OBJECT)).getObject();
            } else {
                args[i++] = convertToJavaObject(arg);
            }
        }
        return args;
    }

    private Object convertToJavaObject(LispObject lispObject) throws LispException {
        if (lispObject.isInstance(LispType.STRING)) {
            return ((LispString)lispObject.cast(LispType.STRING)).getValue();
        }
        if (lispObject.isInstance(LispType.INT)) {
            return ((LispInt)lispObject.cast(LispType.INT)).getValue();
        }
        if (lispObject.isInstance(LispType.FLOAT)) {
            return ((LispFloat)lispObject.cast(LispType.FLOAT)).getValue();
        }
        if (lispObject.isInstance(LispType.BOOL)) {
            return lispObject == LispBool.T;
        }
        if (lispObject.isInstance(LispType.LIST)) {
            LispList lispList = (LispList)lispObject.cast(LispType.LIST);
            Object array[] = new Object[lispList.size()];
            Iterator<LispObject> it = lispList.iterator();
            int i = 0;
            while (it.hasNext()) {
                array[i++] = convertToJavaObject(it.next());
            }
            return array;
        }
        throw new LispException("Cannot convert object of type '%s' to Java object", lispObject.getType().getName());
    }

    protected Class<?>[] getClasses(Object objects[]) {
        Class<?> classes[] = new Class<?>[objects.length];
        for (int i = 0; i < objects.length; i++) {
            classes[i] = objects[i].getClass();
        }
        return classes;
    }

}
