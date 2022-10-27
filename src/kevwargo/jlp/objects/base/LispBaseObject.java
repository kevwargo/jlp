package kevwargo.jlp.objects.base;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.collections.LispList;
import kevwargo.jlp.objects.functions.LispFunction;
import kevwargo.jlp.objects.functions.LispMethod;
import kevwargo.jlp.objects.scalars.LispBool;
import kevwargo.jlp.objects.scalars.LispNil;
import kevwargo.jlp.objects.scalars.LispString;
import kevwargo.jlp.objects.scalars.numbers.LispFloat;
import kevwargo.jlp.objects.scalars.numbers.LispInt;
import kevwargo.jlp.objects.wrappers.LispJavaObject;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class LispBaseObject implements LispObject {

    public static final LispType TYPE = new ObjectType();

    private LispType type;
    private Map<String, LispObject> dict;
    private Map<LispType, LispObject> castMap;

    public LispBaseObject(LispType type) {
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
            return LispNil.NIL;
        }
        if (cls == boolean.class) {
            return LispBool.valueOf((boolean) obj);
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
            for (Object item : (List<?>) obj) {
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

    LispBaseObject() {
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
                if (e.getValue() == this) {
                    return this;
                }
                if (instance != null) {
                    System.err.printf("Warning: %s cast to %s is ambiguous\n", repr(), type.repr());
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

    public LispObject eval(LispRuntime runtime) throws LispException {
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
                return new LispMethod(this, (LispFunction) attr.cast(LispFunction.FUNCTION_TYPE));
            } catch (LispCastException e) {
            }
        }
        return attr;
    }

    public void setAttr(String name, LispObject value) throws LispException {
        dict.put(name, value);
    }

    // main interface method
    public String repr() {
        return String.format("'%s' object at 0x%x", type.getName(), System.identityHashCode(this));
    }

    public boolean bool() {
        return true;
    }

    public String toString() {
        return repr();
    }

    // main interface method
    public Object format() {
        return toString();
    }

    // main interface method
    public Object getJavaObject() {
        return this;
    }

    // main interface method
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
        super("object", new CallArgs());
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        return new LispBaseObject(this);
    }
}
