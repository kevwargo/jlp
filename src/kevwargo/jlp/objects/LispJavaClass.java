package kevwargo.jlp.objects;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.FormalArguments;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

public class LispJavaClass extends LispJavaObject implements LispCallable {

    private static final String ARG_ARGS = "args";
    private static final FormalArguments callArgs = new FormalArguments().rest(ARG_ARGS);

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

    public FormalArguments getFormalArgs() {
        return callArgs;
    }

    public LispObject call(LispRuntime runtime, ArgumentsIterator args) throws LispException {
        Object arguments[] = new Object[args.getLength()];
        Class<?> classes[] = new Class<?>[args.getLength()];
        int index = 0;

        while (args.hasNext()) {
            LispObject obj = args.next();
            arguments[index] = obj.getJavaObject();
            classes[index] = obj.getJavaClass();
            index++;
        }

        Constructor constructor = findConstructor(classes);
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

    private Constructor findConstructor(Class<?> params[]) {
        try {
            return cls.getConstructor(params);
        } catch (NoSuchMethodException exc) {
            for (Constructor c : constructors) {
                if (paramsMatch(c.getParameterTypes(), params)) {
                    return c;
                }
            }
        }

        return null;
    }
}
