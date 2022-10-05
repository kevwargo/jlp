package kevwargo.jlp.objects;

import kevwargo.jlp.exceptions.LispCastException;
import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.ArgumentsIterator;

public class LispString extends LispObject {

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

    public LispObject makeInstance(LispRuntime runtime, ArgumentsIterator arguments)
            throws LispException {
        if (arguments.hasNext()) {
            return new LispString(arguments.next().toString());
        } else {
            return new LispString("");
        }
    }
}
