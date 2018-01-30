package kevwargo.jlp.objects;

import kevwargo.jlp.objects.types.LispType;


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
        return "\"" +
            value.replaceAll("\"", "\\\\\"")
            	.replaceAll("\n", "\\\\n")
            	.replaceAll("\r", "\\\\r")
            	.replaceAll("\t", "\\\\t") +
            "\"";
    }
}
