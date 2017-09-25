package kevwargo.jlp.objects;


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
        return String.format("Java object %s", value.toString());
    }

    public String type() {
        return "javaObject";
    }
}
