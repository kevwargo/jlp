package kevwargo.jlp.objects;

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
}
