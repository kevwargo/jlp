package kevwargo.jlp.object;


public class LispJavaObject extends LispDataObject {

    private Object value;

    public LispJavaObject(Object value) {
        this.value = value;
    }

    public Object getValue() {
        return this.value;
    }

    public void setValue(Object value) {
        this.value = value;
    }

    public String toString() {
        return String.format("Lisp wrapper for Java object %s", value.toString());
    }
}
