package kevwargo.jlp.objects.builtins.javareflect;

import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispFloat;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispList;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.types.LispCastException;
import kevwargo.jlp.objects.types.LispType;

class ReflectedArgs {

    private Object[] objects;
    private Class<?>[] classes;

    ReflectedArgs(LispObject restArgs) throws LispException {
        LispList argList = (LispList)restArgs.cast(LispType.LIST);

        classes = new Class<?>[argList.size()];
        objects = new Object[argList.size()];

        Iterator<LispObject> it = argList.iterator();
        for (int i = 0; it.hasNext(); i++) {
            LispObject arg = it.next();
            classes[i] = getArgClass(arg);
            objects[i] = getArgObject(arg);
        }
    }

    Object[] getObjects() {
        return objects;
    }

    Class<?>[] getClasses() {
        return classes;
    }

    private Class<?> getArgClass(LispObject arg) throws LispException {
        try {
            return ((LispJavaObject)arg.cast(LispType.JAVA_OBJECT)).getObject().getClass();
        } catch (LispCastException e) {}

        if (arg.isInstance(LispType.STRING)) {
            return String.class;
        }
        if (arg.isInstance(LispType.INT)) {
            return Integer.TYPE;
        }
        if (arg.isInstance(LispType.FLOAT)) {
            return Float.TYPE;
        }
        if (arg.isInstance(LispType.BOOL)) {
            return Boolean.TYPE;
        }
        if (arg.isInstance(LispType.LIST)) {
            return Object[].class;
        }

        throw new LispException("Cannot determine a Java class for the object of type '%s'", arg.getType().getName());
    }

    private Object getArgObject(LispObject arg) throws LispException {
        try {
            return ((LispJavaObject)arg.cast(LispType.JAVA_OBJECT)).getObject();
        } catch (LispCastException e) {}

        try {
            return ((LispString)arg.cast(LispType.STRING)).getValue();
        } catch (LispCastException e) {}
        try {
            return (float)((LispFloat)arg.cast(LispType.FLOAT)).getValue();
        } catch (LispCastException e) {}
        try {
            return (int)((LispInt)arg.cast(LispType.INT)).getValue();
        } catch (LispCastException e) {}
        try {
            LispList list = (LispList)arg.cast(LispType.LIST);
            Object[] array = new Object[list.size()];
            Iterator<LispObject> it = list.iterator();
            for (int i = 0; it.hasNext(); i++) {
                array[i] = getArgObject(it.next());
            }
            return array;
        } catch (LispCastException e) {}
        try {
            return ((LispBool)arg.cast(LispType.BOOL)).getValue();
        } catch (LispCastException e) {}

        throw new LispException("Cannot convert an object of type '%s' to a Java object", arg.getType().getName());        
    }

}
