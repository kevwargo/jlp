package kevwargo.jlp.objects;

import kevwargo.jlp.objects.types.LispType;


public class LispFloat extends LispObject {

    private double value;

    LispFloat(double value, LispInt longValue) {
        super(LispType.FLOAT);
        this.value = value;
        if (longValue != null) {
            defineCast(LispType.INT, longValue);
            longValue.defineCast(LispType.FLOAT, this);
        }
    }

    public LispFloat(double value) {
        this(value, new LispInt((long)value, null));
    }

    public double getValue() {
        return value;
    }

    public String repr() {
        return Double.toString(value);
    }

    public Object format() {
        return new Double(value);
    }
    
}
