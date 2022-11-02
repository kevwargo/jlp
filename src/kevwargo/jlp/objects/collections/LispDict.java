package kevwargo.jlp.objects.collections;

import kevwargo.jlp.calls.CallArgs;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispBaseObject;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.base.LispType;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

public class LispDict extends LispBaseObject implements LispCollection {

    public static final LispType TYPE = new DictType();

    private Map<String, LispObject> contents;

    public LispDict(Map<String, LispObject> contents) {
        super(TYPE);
        this.contents = new TreeMap<String, LispObject>(contents);
    }

    public LispDict() {
        this(new TreeMap<String, LispObject>());
    }

    public Map<String, LispObject> getContents() {
        return contents;
    }

    public String repr() {
        StringBuilder sb = new StringBuilder("{");

        Iterator<Map.Entry<String, LispObject>> it = contents.entrySet().iterator();
        boolean first = true;
        while (it.hasNext()) {
            Map.Entry<String, LispObject> entry = it.next();
            String key = new LispString(entry.getKey()).repr();
            String value = entry.getValue().repr();

            if (!first) {
                sb.append(", ");
            }
            first = false;

            sb.append(key);
            sb.append(": ");
            sb.append(value);
        }

        return sb.append("}").toString();
    }

    public LispObject getItem(LispObject keyObj) throws LispException {
        if (!keyObj.isInstance(LispString.TYPE)) {
            throw new LispException("Currently only strings are supported as dict keys");
        }

        String key = ((LispString) keyObj.cast(LispString.TYPE)).getValue();
        LispObject value = contents.get(key);
        if (value != null) {
            return value;
        }

        throw new LispException("No such key: %s", key);
    }
}

class DictType extends LispType {

    // public static final String ARG_SEQ = "seq";
    public static final String ARG_KEYS = "keys";

    private static final CallArgs args = new CallArgs().otherKeys(ARG_KEYS);

    DictType() {
        super("dict", new LispType[] {LispCollection.TYPE}, args);
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        return args.get(ARG_KEYS);
    }
}
