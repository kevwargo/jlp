package kevwargo.jlp.objects;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.LispNamespace;


public class LispJavaObject extends LispObject {

    private Object obj;
    private Class<?> cls;
    private Map<String, Field> fields;
    private Map<String, MethodProxy> methods;

    public LispJavaObject(Object obj) {
        this(obj, obj.getClass());
    }

    public LispJavaObject(Class<?> cls) {
        this(null, cls);
    }

    private LispJavaObject(Object obj, Class<?> cls) {
        super(LispType.JAVA_OBJECT);

        this.obj = obj;
        this.cls = cls;

        initClass();
    }

    public Object getObject() {
        return obj;
    }

    public String repr() {
        return obj.toString();
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
            return new LispJavaObject(f.get(obj));
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

    private class MethodProxy extends LispObject {

        private List<Method> methods;

        public MethodProxy(Method m) {
            methods = new ArrayList<Method>();
            methods.add(m);
        }

        public void addMethod(Method m) {
            methods.add(m);
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
