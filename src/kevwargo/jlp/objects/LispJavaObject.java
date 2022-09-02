package kevwargo.jlp.objects;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class LispJavaObject extends LispObject {

    private Object obj;
    private Class<?> cls;
    private Map<String, Field> fields;
    private Map<String, MethodProxy> methods;
    private Constructor<?>[] constructors;

    public LispJavaObject(Object obj) {
        this(obj, obj.getClass());
    }

    protected LispJavaObject(Object obj, Class<?> cls) {
        super(LispType.JAVA_OBJECT);

        this.obj = obj;
        this.cls = cls;

        initClass();
    }

    public Object getObject() {
        if (obj != null) {
            return obj;
        }
        return cls;
    }

    public String repr() {
        return toString();
    }

    public String toString() {
        if (obj == null) {
            return cls.toString();
        }
        return obj.toString();
    }

    public Object format() {
        return obj;
    }

    public Object getJavaObject() {
        return obj;
    }

    public Class<?> getJavaClass() {
        return cls;
    }

    public LispObject getAttr(String name) {
        if (!name.startsWith("#")) {
            return methods.get(name);
        }

        Field f = fields.get(name.substring(1));
        if (f == null) {
            return null;
        }

        try {
            return LispObject.wrap(f.get(obj), f.getType());
        } catch (IllegalAccessException e) {
            return null;
        }
    }

    public void setAttr(String name, LispObject value) throws LispException {
        if (!name.startsWith("#")) {
            throw new LispException("Cannot overwrite Java method");
        };

        name = name.substring(1);
        Field f = fields.get(name);
        if (f == null) {
            throw new LispException("Cannot set a nonexistent '%s' Java field", name);
        }

        try {
            f.set(obj, value.getJavaObject());
        } catch (IllegalAccessException e) {
            throw new LispException(e);
        }
    }

    public LispObject call(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        if (obj != null) {
            throw new LispException("The instance of '%s' is not callable", cls);
        }

        throw new LispException("Instantiating Java classes is not yet implemented");
    }


    private void initClass() {
        fields = new HashMap<String, Field>();
        methods = new HashMap<String, MethodProxy>();

        for (Field f : cls.getFields()) {
            fields.put(f.getName(), f);
        }

        for (Method m : cls.getMethods()) {
            MethodProxy method = methods.get(m.getName());
            if (method == null) {
                methods.put(m.getName(), new MethodProxy(m));
            } else {
                method.addMethod(m);
            }
        }

        constructors = cls.getConstructors();
    }

    private class MethodProxy extends LispFunction {

        private static final String ARG_PARAMS = "params";

        private List<Method> methods;
        private String name;

        public MethodProxy(Method m) {
            super(m.getName(), new FormalArguments().rest(ARG_PARAMS));
            name = m.getName();
            methods = new ArrayList<Method>();
            methods.add(m);
        }

        public void addMethod(Method m) {
            methods.add(m);
        }

        public String repr() {
            return String.format("Java method %s.%s", LispJavaObject.this.cls, name);
        }

        // No varargs
        // No choosing the nearest by inheritance distance between actual vs declared arguments
        protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
            LispList argList = (LispList)arguments.get(ARG_PARAMS).cast(LispType.LIST);
            Object args[] = new Object[argList.size()];
            Class<?> classes[] = new Class<?>[argList.size()];
            int index = 0;

            for (LispObject obj : argList) {
                args[index] = obj.getJavaObject();
                classes[index] = obj.getJavaClass();
                index++;
            }

            Method foundMethod = null;
            try {
                foundMethod = LispJavaObject.this.cls.getMethod(name, classes);
            } catch (NoSuchMethodException exc) {
                for (Method m : methods) {
                    Class<?> paramTypes[] = m.getParameterTypes();
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
                        foundMethod = m;
                        break;
                    }
                }
            }


            if (foundMethod == null) {
                throw new LispException("No method '%s' for the provided arguments", name);
            }

            try {
                Object result = foundMethod.invoke(LispJavaObject.this.obj, args);
                return LispObject.wrap(result, foundMethod.getReturnType());
            } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException | NullPointerException | ExceptionInInitializerError exc) {
                throw new LispException(exc);
            }

        }

    }

}

class JavaObjectType extends LispType {

    JavaObjectType() {
        super("java-object", new LispType[] { OBJECT });
    }

    public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        if (arguments.hasNext()) {
            throw new LispException("java-object's constructor does not accept any arguments");
        }

        return new LispJavaObject(new Object());
    }

}
