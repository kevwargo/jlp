package kevwargo.jlp.objects;

import kevwargo.jlp.objects.types.LispType;


public class LispInt extends LispObject {

    private long value;

    public LispInt(long value) {
        super(LispType.INT);
        this.value = value;
    }

    public long getValue() {
        return value;
    }

    public String repr() {
        return Long.toString(value);
    }

    public Object format() {
        return new Long(value);
    }

}
