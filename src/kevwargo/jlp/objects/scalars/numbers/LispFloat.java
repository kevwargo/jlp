package kevwargo.jlp.objects.scalars.numbers;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.objects.base.LispBaseObject;
import kevwargo.jlp.objects.base.LispObject;
import kevwargo.jlp.objects.base.LispType;
import kevwargo.jlp.objects.scalars.LispString;
import kevwargo.jlp.objects.wrappers.LispJavaObject;
import kevwargo.jlp.runtime.LispNamespace.Layer;
import kevwargo.jlp.runtime.LispRuntime;

public class LispFloat extends LispBaseObject implements LispNumber {

    public static final LispType TYPE = new FloatType();

    private double value;
    private Class<?> cls;

    LispFloat(double value, LispInt intCast, Class<?> cls) {
        super(LispFloat.TYPE);
        this.value = value;
        this.cls = cls;
        if (intCast != null) {
            defineCast(LispInt.TYPE, intCast);
            intCast.defineCast(LispFloat.TYPE, this);
        }
    }

    public LispFloat(double value) {
        this(value, new LispInt((long) value, null, long.class), double.class);
    }

    public LispFloat(float value) {
        this(value, new LispInt((long) value, null, long.class), float.class);
    }

    public double getValue() {
        return value;
    }

    public String repr() {
        return Double.toString(value);
    }

    public boolean bool() {
        return value != 0.0;
    }

    public Object format() {
        return Double.valueOf(value);
    }

    public Object getJavaObject() {
        if (cls == float.class) {
            return (float) value;
        }
        return value;
    }

    public Class<?> getJavaClass() {
        return cls;
    }

    public double getDoubleValue() {
        return value;
    }
}

class FloatType extends LispType {

    FloatType() {
        super("float", new LispType[] {LispNumber.TYPE});
    }

    public LispObject call(LispRuntime runtime, Layer args) throws LispException {
        if (!args.containsKey(ARG_OBJ)) {
            return new LispFloat(0.0);
        }

        LispObject object = args.get(ARG_OBJ);
        if (object.isInstance(LispInt.TYPE)) {
            return new LispFloat((double) ((LispInt) object.cast(LispInt.TYPE)).getValue());
        }

        if (object.isInstance(LispFloat.TYPE)) {
            return new LispFloat(((LispFloat) object.cast(LispFloat.TYPE)).getValue());
        }

        if (object.isInstance(LispString.TYPE)) {
            return new LispFloat(
                    Double.parseDouble(((LispString) object.cast(LispString.TYPE)).getValue()));
        }

        if (object.isInstance(LispJavaObject.TYPE)) {
            Object javaNumber = ((LispJavaObject) object).getObject();
            if (javaNumber instanceof Integer) {
                return new LispFloat(((Integer) javaNumber).doubleValue());
            }
            if (javaNumber instanceof Long) {
                return new LispFloat(((Long) javaNumber).doubleValue());
            }
            if (javaNumber instanceof Float) {
                return new LispFloat(((Float) javaNumber).doubleValue());
            }
            if (javaNumber instanceof Double) {
                return new LispFloat(((Double) javaNumber).doubleValue());
            }
        }

        throw new LispException("Object '%s' cannot be converted to float", object);
    }
}
