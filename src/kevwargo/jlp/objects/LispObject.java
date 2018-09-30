package kevwargo.jlp.objects;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.types.LispCastException;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.LispNamespace;


public class LispObject {

    protected LispType type;
    protected Map<String, LispObject> dict;

    private Map<LispType, LispObject> castMap;


    public LispObject() {
        dict = new HashMap<String, LispObject>();
        castMap = new HashMap<LispType, LispObject>();
    }

    public LispType getType() {
        return type;
    }

    public void setType(LispType type) {
        this.type = type;
        defineCast(type, this);
    }

    public boolean isInstance(LispType type) {
        return this.type.isSubtype(type);
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
        if (attr != null && attr.isInstance(LispType.FUNCTION)) {
            try {
                return new LispMethod(this, (LispFunction)attr.cast(LispType.FUNCTION));
            } catch (LispCastException e) {
                // should not happen
                return null;
            }
        }
        return attr;
    }

    private LispObject getAttr(String name, LispType type) {
        if (type.dict.containsKey(name)) {
            return type.dict.get(name);
        }
        for (LispType baseType : type.getBaseTypes()) {
            LispObject attr = getAttr(name, baseType);
            if (attr != null) {
                return attr;
            }
        }
        return null;
    }

    public void setAttr(String name, LispObject value) {
        dict.put(name, value);
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
                    System.err.printf("Warning: '%s' cast to '%s' is ambiguous\n", this.toString(), type.toString());
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

}
