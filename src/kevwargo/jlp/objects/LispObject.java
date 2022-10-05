package kevwargo.jlp.objects;

import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.utils.ArgumentsIterator;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class LispObject {

    private LispType type;
    private Map<String, LispObject> dict;
    private Map<LispType, LispObject> castMap;

    public LispObject(LispType type) {
        this();
        setType(type);
    }

    /**
     * Wraps a Java object. The cls parameter needs to be provided so we don't lose the precise type
     * information when passing primitive types to the Object parameter (e.g. if we pass an int to
     * the parameter declared as Object, the param.getClass() inside the method will return
     * java.lang.Integer and not int.class)
     */
    // TODO: arrays
    public static LispObject wrap(Object obj, Class<?> cls) {
        if (obj == null) {
            return LispBool.NIL;
        }
        if (cls == boolean.class) {
            if ((boolean) obj) {
                return LispBool.T;
            }
            return LispBool.NIL;
        }

        if (cls == long.class) {
            return new LispInt((long) obj);
        }
        if (cls == int.class) {
            return new LispInt((int) obj);
        }
        if (cls == short.class) {
            return new LispInt((short) obj);
        }
        if (cls == char.class) {
            return new LispInt((char) obj);
        }
        if (cls == byte.class) {
            return new LispInt((byte) obj);
        }

        if (cls == float.class) {
            return new LispFloat((float) obj);
        }
        if (cls == double.class) {
            return new LispFloat((double) obj);
        }

        if (obj instanceof String) {
            return new LispString((String) obj);
        }

        if (obj instanceof List) {
            LispList list = new LispList();
            for (Object item : (List) obj) {
                list.add(wrap(item));
            }
            return list;
        }

        if (cls.isArray()) {
            LispList list = new LispList();
            for (Object item : (Object[]) obj) {
                list.add(wrap(item, cls.getComponentType()));
            }
            return list;
        }

        return new LispJavaObject(obj);
    }

    public static LispObject wrap(Object obj) {
        return wrap(obj, obj.getClass());
    }

    LispObject() {
        dict = new HashMap<String, LispObject>();
        castMap = new HashMap<LispType, LispObject>();
    }

    void setType(LispType type) {
        this.type = type;
        defineCast(type, this);
    }

    public LispType getType() {
        return type;
    }

    public Map<String, LispObject> getDict() {
        return dict;
    }

    public boolean isInstance(LispType type) {
        return this.type.isSubtype(type);
    }

    public void defineCast(LispType type, LispObject instance) {
        castMap.put(type, instance);
    }

    public boolean isCastDefined(LispType type) {
        return castMap.containsKey(type);
    }

    public LispObject cast(LispType type) throws LispCastException {
        LispObject instance = null;
        for (Map.Entry<LispType, LispObject> e : castMap.entrySet()) {
            if (e.getKey().isSubtype(type)) {
                if (instance != null) {
                    System.err.printf(
                            "Warning: '%s' cast to '%s' is ambiguous\n",
                            toString(), type.toString());
                } else {
                    instance = e.getValue();
                }
            }
        }
        if (instance == null) {
            throw new LispCastException(
                    "object '%s' cannot be converted to '%s'", toString(), type.getName());
        }
        return instance;
    }

    public LispObject eval(LispNamespace namespace) throws LispException {
        return this;
    }

    public LispObject getAttr(String name) {
        LispObject attr = dict.get(name);
        if (attr != null) {
            return attr;
        }

        attr = getAttr(name, type);
        if (attr != null) {
            try {
                return new LispMethod(this, (LispFunction) attr.cast(LispType.FUNCTION));
            } catch (LispCastException e) {
            }
        }
        return attr;
    }

    public void setAttr(String name, LispObject value) throws LispException {
        dict.put(name, value);
    }

    public LispObject call(LispNamespace namespace, ArgumentsIterator arguments)
            throws LispException {
        LispObject callable = getAttr("@call@");
        if (callable != null) {
            return callable.call(namespace, arguments);
        }
        throw new LispException("'%s' object is not callable", type.getName());
    }

    public String repr() {
        return String.format("'%s' object at 0x%x", type.getName(), System.identityHashCode(this));
    }

    public String toString() {
        return repr();
    }

    public Object format() {
        return toString();
    }

    public Object getJavaObject() {
        return this;
    }

    public Class<?> getJavaClass() {
        return getClass();
    }

    private LispObject getAttr(String name, LispType type) {
        if (type.getDict().containsKey(name)) {
            return type.getDict().get(name);
        }
        for (LispType baseType : type.getBases()) {
            LispObject attr = getAttr(name, baseType);
            if (attr != null) {
                return attr;
            }
        }
        return null;
    }
}

class ObjectType extends LispType {

    ObjectType() {
        super("object");
    }

    public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments)
            throws LispException {
        return new LispObject(this);
    }
}
