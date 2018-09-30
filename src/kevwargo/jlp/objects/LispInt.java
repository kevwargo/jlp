package kevwargo.jlp.objects;

import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.objects.types.TypeInitializer;


public class LispInt extends LispObject {

    private long value;

    LispInt(long value, LispFloat floatCast) {
        super();
        TypeInitializer.instance().deferTypeSet(this, "int");
        this.value = value;
        if (floatCast != null) {
            TypeInitializer.instance().deferCastDefine(this, "float", floatCast);
            TypeInitializer.instance().deferCastDefine(floatCast, "int", this);
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
