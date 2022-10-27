package kevwargo.jlp.objects.wrappers;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispBaseObject;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.functions.LispCallable;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

public class LispJavaClass extends LispJavaObject implements LispCallable {

    private static final String ARG_ARGS = "args";
    private static final CallArgs callArgs = new CallArgs().rest(ARG_ARGS);

    private Constructor<?> constructors[];

    public static LispJavaClass forName(String name) {
        try {
            return new LispJavaClass(name);
        } catch (ClassNotFoundException exc) {
            return null;
        }
    }

    private LispJavaClass(String name) throws ClassNotFoundException {
        super(null, resolveClass(name));
        constructors = cls.getConstructors();
    }

    public Object getObject() {
        return cls;
    }

    public String toString() {
        return cls.getName();
    }

    public Object format() {
        return cls;
    }

    public Object getJavaObject() {
        return cls;
    }

    public Class<?> getWrappedClass() {
        return cls;
    }

    public Class<?> getJavaClass() {
        return cls.getClass();
    }

    private static Class<?> resolveClass(String name) throws ClassNotFoundException {
        try {
            return Class.forName(name);
        } catch (ClassNotFoundException e) {
            if (name.indexOf('.') == -1) {
                return Class.forName("java.lang." + name);
            }
            throw e;
        }
    }

    public CallArgs getCallArgs() {
        return callArgs;
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        LispList params = (LispList) args.get(ARG_ARGS);

        Object arguments[] = new Object[params.size()];
        Class<?> classes[] = new Class<?>[params.size()];
        for (int i = 0; i < params.size(); i++) {
            arguments[i] = params.get(i).getJavaObject();
            classes[i] = params.get(i).getJavaClass();
        }

        Constructor<?> constructor = findConstructor(classes);
        if (constructor == null) {
            throw new LispException(
                    "%s has no constructor for the provided arguments: %s",
                    cls.getName(), describeClasses(classes));
        }

        try {
            Object result = constructor.newInstance(arguments);
            return LispBaseObject.wrap(result, cls);
        } catch (IllegalAccessException
                | IllegalArgumentException
                | InstantiationException
                | InvocationTargetException
                | ExceptionInInitializerError exc) {
            throw new LispException(exc);
        }
    }

    private Constructor<?> findConstructor(Class<?> params[]) {
        try {
            return cls.getConstructor(params);
        } catch (NoSuchMethodException exc) {
            for (Constructor<?> c : constructors) {
                if (paramsMatch(c.getParameterTypes(), params)) {
                    return c;
                }
            }
        }

        return null;
    }
}
