package kevwargo.jlp.objects;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.FormalArguments;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class LispJavaObject extends LispBaseObject {

    private Object obj;
    protected Class<?> cls;
    private Map<String, Field> fields;
    private Map<String, MethodProxy> methods;

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
        return obj;
    }

    public String repr() {
        return toString();
    }

    public String toString() {
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
            return LispBaseObject.wrap(f.get(obj), f.getType());
        } catch (IllegalAccessException e) {
            return null;
        }
    }

    public void setAttr(String name, LispObject value) throws LispException {
        if (!name.startsWith("#")) {
            throw new LispException("Cannot overwrite Java method");
        }

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

    public LispObject call(LispRuntime runtime, ArgumentsIterator arguments) throws LispException {
        throw new LispException("The instance of '%s' is not callable", cls);
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

        // TODO: varargs
        // TODO: choose the nearest method by the inheritance distance between actual vs declared
        // arguments
        protected LispObject callInternal(LispRuntime runtime, Map<String, LispObject> args)
                throws LispException {
            LispList argList = (LispList) args.get(ARG_PARAMS).cast(LispType.LIST);
            Object arguments[] = new Object[argList.size()];
            Class<?> classes[] = new Class<?>[argList.size()];
            int index = 0;

            for (LispObject obj : argList) {
                arguments[index] = obj.getJavaObject();
                classes[index] = obj.getJavaClass();
                index++;
            }

            Method method = LispJavaObject.this.findMethod(name, classes, methods);
            if (method == null) {
                throw new LispException(
                        "'%s' has no method '%s' for the provided arguments %s",
                        LispJavaObject.this.cls.getName(), name, describeClasses(classes));
            }

            try {
                Object result = method.invoke(LispJavaObject.this.obj, arguments);
                return LispBaseObject.wrap(result, method.getReturnType());
            } catch (IllegalAccessException
                    | IllegalArgumentException
                    | InvocationTargetException
                    | NullPointerException
                    | ExceptionInInitializerError exc) {
                throw new LispException(exc);
            }
        }
    }

    protected static boolean paramsMatch(Class<?> declared[], Class<?> actual[]) {
        if (declared.length != actual.length) {
            return false;
        }

        for (int i = 0; i < declared.length; i++) {
            if (!declared[i].isAssignableFrom(actual[i])) {
                return false;
            }
        }

        return true;
    }

    protected static String describeClasses(Class<?> params[]) {
        StringBuilder sb = new StringBuilder("[");

        int index = 0;
        if (params.length > index) {
            sb.append(params[index].getName());
            index++;
        }

        while (params.length > index) {
            sb.append(", ");
            sb.append(params[index].getName());
            index++;
        }

        sb.append("]");
        return sb.toString();
    }

    private Method findMethod(String name, Class<?> params[], List<Method> methods)
            throws LispException {
        try {
            return cls.getMethod(name, params);
        } catch (NoSuchMethodException exc) {
            for (Method m : methods) {
                if (paramsMatch(m.getParameterTypes(), params)) {
                    return m;
                }
            }
        }

        return null;
    }
}

class JavaObjectType extends LispType {

    JavaObjectType() {
        super("java-object", new LispType[] {OBJECT});
    }

    public LispObject makeInstance(LispRuntime runtime, ArgumentsIterator arguments)
            throws LispException {
        if (arguments.hasNext()) {
            throw new LispException("java-object's constructor does not accept any arguments");
        }

        return new LispJavaObject(new Object());
    }
}
