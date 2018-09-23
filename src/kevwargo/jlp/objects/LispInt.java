package kevwargo.jlp.objects;

import kevwargo.jlp.objects.types.LispType;


public class LispInt extends LispObject {

    private long value;

    LispInt(long value, LispFloat floatCast) {
        super(LispType.INT);
        this.value = value;
        if (floatCast != null) {
            defineCast(LispType.FLOAT, floatCast);
            floatCast.defineCast(LispType.INT, this);
        }
    }

    public LispInt(long value) {
        this(value, new LispFloat((double)value, null));
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
