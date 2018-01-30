package kevwargo.jlp.objects;

import kevwargo.jlp.objects.types.LispType;


public class LispFloat extends LispObject {

    private double value;

    public LispFloat(double value) {
        super(LispType.FLOAT);
        this.value = value;
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
