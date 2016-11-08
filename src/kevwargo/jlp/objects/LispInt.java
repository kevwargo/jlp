package kevwargo.jlp.objects;


public class LispInt extends LispDataObject {

    private long value;

    public LispInt(long value) {
        this.value = value;
    }

    public long getValue() {
        return this.value;
    }

    public void setValue(long value) {
        this.value = value;
    }

    public String toString() {
        return String.format("%d", value);
    }
}
