package kevwargo.jlp.objects;

import kevwargo.jlp.LispException;


public class LispJavaClass extends LispJavaObject {

    public LispJavaClass(String name) throws LispException {
        super(null, resolveClass(name));
    }

    private static Class<?> resolveClass(String name) throws LispException {
        Class<?> cls = null;
        try {
            cls = Class.forName(name);
        } catch (ClassNotFoundException e) {}
        if (cls == null) {
            try {
                cls = Class.forName("java.lang." + name);
            } catch (ClassNotFoundException e) {}
        }

        if (cls == null) {
            throw new LispException("Java class '%s' not found", name);
        }

        return cls;
    }

}
