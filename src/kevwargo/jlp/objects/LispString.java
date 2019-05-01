package kevwargo.jlp.objects;

import kevwargo.jlp.objects.types.LispCastException;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.objects.types.TypeInitializer;


public class LispString extends LispObject {

    private String value;

    public LispString(String value) {
        super();
        TypeInitializer.instance().deferTypeSet(this, "string");
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    public String toString() {
        return value;
    }

    public String repr() {
        return "\"" +
            value.replaceAll("\"", "\\\\\"")
            	.replaceAll("\n", "\\\\n")
            	.replaceAll("\r", "\\\\r")
            	.replaceAll("\t", "\\\\t") +
            "\"";
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
        LispObject object = (LispObject)other;
        try {
            return value.equals(((LispString)object.cast(LispType.STRING)).getValue());
        } catch (LispCastException e) {
            return false;
        }
    }
}
