package kevwargo.jlp.objects;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import kevwargo.jlp.LispException;
import kevwargo.jlp.LispCastException;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.LispNamespace;


public class LispObject {

    private LispType type;
    private Map<String, LispObject> dict;
    private Map<LispType, LispObject> castMap;

    public LispObject(LispType type) {
        this();
        setType(type);
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
                    System.err.printf("Warning: '%s' cast to '%s' is ambiguous\n", toString(), type.toString());
                } else {
                    instance = e.getValue();
                }
            }
        }
        if (instance == null) {
            throw new LispCastException("object '%s' cannot be converted to '%s'", toString(), type.getName());
        }
        return instance;
    }

    public LispObject eval(LispNamespace namespace) throws LispException {
        return this;
    }

    public LispObject getAttr(String name, boolean withDict) {
        LispObject attr;
        if (withDict) {
            attr = dict.get(name);
            if (attr != null) {
                return attr;
            }
        }
        attr = getAttr(name, type);
        if (attr != null) {
            try {
                return new LispMethod(this, (LispFunction)attr.cast(LispType.FUNCTION));
            } catch (LispCastException e) {}
        }
        return attr;
    }

    public void setAttr(String name, LispObject value) {
        dict.put(name, value);
    }

    public LispObject call(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        LispObject callable = getAttr("@call@", false);
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

    public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        return new LispObject(this);
    }

}
