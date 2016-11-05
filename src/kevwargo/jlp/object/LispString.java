package kevwargo.jlp.object;


public class LispString extends LispDataObject {

    private String value;

    public LispString(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public String toString() {
        return String.format("Lisp string \"%s\"", value);
    }
}
