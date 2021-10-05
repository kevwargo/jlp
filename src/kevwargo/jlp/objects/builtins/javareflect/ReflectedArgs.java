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
            classes[i] = arg.getJavaClass();
            objects[i] = arg.getJavaObject();
        }
    }

    Object[] getObjects() {
        return objects;
    }

    Class<?>[] getClasses() {
        return classes;
    }

}
