package kevwargo.jlp.objects;

import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class LispString extends LispBaseObject implements LispIterable {

    private String value;

    public LispString(String value) {
        super(LispType.STRING);
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    public String toString() {
        return value;
    }

    public String repr() {
        return "\""
                + value.replaceAll("\"", "\\\\\"")
                        .replaceAll("\n", "\\\\n")
                        .replaceAll("\r", "\\\\r")
                        .replaceAll("\t", "\\\\t")
                + "\"";
    }

    public boolean bool() {
        return !value.isEmpty();
    }

    public int hashCode() {
        return value.hashCode();
    }

    public boolean equals(Object other) {
        if (other == null) {
            return false;
        }
        if (!(other instanceof LispObject)) {
            return false;
        }
        LispObject object = (LispObject) other;
        try {
            return value.equals(((LispString) object.cast(LispType.STRING)).getValue());
        } catch (LispCastException e) {
            return false;
        }
    }

    public Iterator<LispObject> iterator() {
        return new StringIterator(value);
    }

    public Object getJavaObject() {
        return value;
    }

    public Class<?> getJavaClass() {
        return String.class;
    }
}

class StringType extends LispType {

    StringType() {
        super("str", new LispType[] {OBJECT});
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        if (!args.containsKey(ARG_OBJ)) {
            return new LispString("");
        }

        return new LispString(args.get(ARG_OBJ).toString());
    }
}

class StringIterator implements Iterator<LispObject> {

    private String value;
    private int pos;

    StringIterator(String value) {
        this.value = value;
    }

    public boolean hasNext() {
        return pos < value.length();
    }

    public LispObject next() throws NoSuchElementException {
        if (!hasNext()) {
            throw new NoSuchElementException();
        }

        return new LispInt(value.charAt(pos++));
    }
}
