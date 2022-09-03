package kevwargo.jlp.objects;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.LispNamespace;


public class LispJavaClass extends LispJavaObject {

    private Constructor<?> constructors[];

    public static LispJavaClass forName(String name) {
        try {
            return new LispJavaClass(name);
        } catch (LispException exc) {
            return null;
        }
    }

    private LispJavaClass(String name) throws LispException {
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

    private static Class<?> resolveClass(String name) throws LispException {
        try {
            return Class.forName(name);
        } catch (ClassNotFoundException e) {}
        try {
            return Class.forName("java.lang." + name);
        } catch (ClassNotFoundException e) {}

        throw new LispException("Java class '%s' not found", name);
    }

    public LispObject call(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        Object args[] = new Object[arguments.getLength()];
        Class<?> classes[] = new Class<?>[arguments.getLength()];
        int index = 0;

        while (arguments.hasNext()) {
            LispObject obj = arguments.next();
            args[index] = obj.getJavaObject();
            classes[index] = obj.getJavaClass();
            index++;
        }

        Constructor foundConstructor = null;
        try {
            foundConstructor = cls.getConstructor(classes);
        } catch (NoSuchMethodException exc) {
            for (Constructor c : constructors) {
                Class<?> paramTypes[] = c.getParameterTypes();
                if (paramTypes.length != classes.length) {
                    continue;
                }

                index = 0;
                while (index < paramTypes.length && index < classes.length) {
                    if (!paramTypes[index].isAssignableFrom(classes[index])) {
                        break;
                    }
                    index++;
                }
                if (paramTypes.length == index) {
                    foundConstructor = c;
                    break;
                }
            }
        }


        if (foundConstructor == null) {
            throw new LispException("%s has no constructor for the provided arguments: %s", cls.getName());
        }

        try {
            Object result = foundConstructor.newInstance(args);
            return LispObject.wrap(result, cls);
        } catch (IllegalAccessException | IllegalArgumentException | InstantiationException | InvocationTargetException | ExceptionInInitializerError exc) {
            throw new LispException(exc);
        }
    }

}
