package kevwargo.jlp.objects;

import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.LispNamespace;


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

    public Object getJavaObject() {
        return (int)value;
    }

    public Class<?> getJavaClass() {
        return Integer.TYPE;
    }

}

class IntType extends LispType {

    IntType() {
        super("int", new LispType[] { OBJECT });
    }

    public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        if (!arguments.hasNext()) {
            return new LispInt(0);
        }

        LispObject object = arguments.next();
        if (object.isInstance(INT)) {
            return new LispInt(((LispInt)object.cast(INT)).getValue());
        }

        if (object.isInstance(FLOAT)) {
            return new LispInt((long)((LispFloat)object.cast(FLOAT)).getValue());
        }

        if (object.isInstance(STRING)) {
            int radix = 10;
            if (arguments.hasNext()) {
                radix = (int)((LispInt)arguments.next().cast(INT)).getValue();
            }
            return new LispInt(Long.parseLong(((LispString)object.cast(STRING)).getValue(), radix));
        }

        if (object.isInstance(JAVA_OBJECT)) {
            Object javaNumber = ((LispJavaObject)object).getObject();
            if (javaNumber instanceof Integer) {
                return new LispInt(((Integer)javaNumber).longValue());
            }
            if (javaNumber instanceof Long) {
                return new LispInt(((Long)javaNumber).longValue());
            }
            if (javaNumber instanceof Float) {
                return new LispInt(((Float)javaNumber).longValue());
            }
            if (javaNumber instanceof Double) {
                return new LispInt(((Double)javaNumber).longValue());
            }
        }

        throw new LispException("Object '%s' cannot be converted to int", object);
    }

}
