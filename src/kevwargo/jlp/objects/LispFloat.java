package kevwargo.jlp.objects;

import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.objects.types.TypeInitializer;


public class LispFloat extends LispObject {

    private double value;

    LispFloat(double value, LispInt intCast) {
        super();
        TypeInitializer.instance().deferTypeSet(this, "float");
        this.value = value;
        if (intCast != null) {
            TypeInitializer.instance().deferCastDefine(this, "int", intCast);
            TypeInitializer.instance().deferCastDefine(intCast, "float", this);
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
