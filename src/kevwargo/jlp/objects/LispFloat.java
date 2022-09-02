package kevwargo.jlp.objects;

import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.LispNamespace;


public class LispFloat extends LispObject {

    private double value;
    private Class<?> cls;

    LispFloat(double value, LispInt intCast, Class<?> cls) {
        super(LispType.FLOAT);
        this.value = value;
        this.cls = cls;
        if (intCast != null) {
            defineCast(LispType.INT, intCast);
            intCast.defineCast(LispType.FLOAT, this);
        }
    }

    public LispFloat(double value) {
        this(value, new LispInt((long)value, null, long.class), double.class);
    }

    public LispFloat(float value) {
        this(value, new LispInt((long)value, null, long.class), float.class);
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

    public Object getJavaObject() {
        if (cls == float.class) {
            return (float)value;
        }
        return value;
    }

    public Class<?> getJavaClass() {
        return cls;
    }

}

class FloatType extends LispType {

    FloatType() {
        super("float", new LispType[] { OBJECT });
    }

    public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        if (!arguments.hasNext()) {
            return new LispFloat(0.0);
        }

        LispObject object = arguments.next();
        if (object.isInstance(INT)) {
            return new LispFloat((double)((LispInt)object.cast(INT)).getValue());
        }

        if (object.isInstance(FLOAT)) {
            return new LispFloat(((LispFloat)object.cast(FLOAT)).getValue());
        }

        if (object.isInstance(STRING)) {
            return new LispFloat(Double.parseDouble(((LispString)object.cast(STRING)).getValue()));
        }

        if (object.isInstance(JAVA_OBJECT)) {
            Object javaNumber = ((LispJavaObject)object).getObject();
            if (javaNumber instanceof Integer) {
                return new LispFloat(((Integer)javaNumber).doubleValue());
            }
            if (javaNumber instanceof Long) {
                return new LispFloat(((Long)javaNumber).doubleValue());
            }
            if (javaNumber instanceof Float) {
                return new LispFloat(((Float)javaNumber).doubleValue());
            }
            if (javaNumber instanceof Double) {
                return new LispFloat(((Double)javaNumber).doubleValue());
            }
        }

        throw new LispException("Object '%s' cannot be converted to float", object);
    }

}
