package kevwargo.jlp.objects;

import kevwargo.jlp.exceptions.LispException;
import kevwargo.jlp.runtime.LispNamespace;
import kevwargo.jlp.runtime.LispRuntime;
import kevwargo.jlp.utils.CallArgs;

public class LispInt extends LispBaseObject {

    private long value;
    private Class<?> cls;

    public LispInt(long value) {
        this(value, new LispFloat((double) value, null, double.class), long.class);
    }

    public LispInt(int value) {
        this((long) value, new LispFloat((double) value, null, double.class), int.class);
    }

    public LispInt(short value) {
        this((long) value, new LispFloat((double) value, null, double.class), short.class);
    }

    public LispInt(char value) {
        this((long) value, new LispFloat((double) value, null, double.class), char.class);
    }

    public LispInt(byte value) {
        this((long) value, new LispFloat((double) value, null, double.class), byte.class);
    }

    LispInt(long value, LispFloat floatCast, Class<?> cls) {
        super(LispType.INT);
        this.cls = cls;
        this.value = value;
        if (floatCast != null) {
            defineCast(LispType.FLOAT, floatCast);
            floatCast.defineCast(LispType.INT, this);
        }
    }

    public long getValue() {
        return value;
    }

    public String repr() {
        return Long.toString(value);
    }

    public boolean bool() {
        return value != 0;
    }

    public Object format() {
        return Long.valueOf(value);
    }

    public Object getJavaObject() {
        if (cls == int.class) {
            return (int) value;
        }
        if (cls == short.class) {
            return (short) value;
        }
        if (cls == char.class) {
            return (char) value;
        }
        if (cls == byte.class) {
            return (byte) value;
        }
        return value;
    }

    public Class<?> getJavaClass() {
        return cls;
    }
}

class IntType extends LispType {

    private static final String ARG_RADIX = "radix";

    IntType() {
        super("int", new LispType[] {OBJECT}, new CallArgs().opt(ARG_OBJ).opt(ARG_RADIX));
    }

    public LispObject call(LispRuntime runtime, LispNamespace.Layer args) throws LispException {
        if (!args.containsKey(ARG_OBJ)) {
            return new LispInt(0);
        }

        LispObject object = args.get(ARG_OBJ);
        if (object.isInstance(INT)) {
            return new LispInt(((LispInt) object.cast(INT)).getValue());
        }

        if (object.isInstance(FLOAT)) {
            return new LispInt((long) ((LispFloat) object.cast(FLOAT)).getValue());
        }

        if (object.isInstance(STRING)) {
            String number = ((LispString) object.cast(STRING)).getValue();
            int radix = 10;
            if (args.containsKey(ARG_RADIX)) {
                radix = (int) ((LispInt) args.get(ARG_RADIX).cast(INT)).getValue();
            }
            return new LispInt(Long.parseLong(number, radix));
        }

        if (object.isInstance(JAVA_OBJECT)) {
            Object javaNumber = ((LispJavaObject) object).getObject();
            if (javaNumber instanceof Integer) {
                return new LispInt(((Integer) javaNumber).longValue());
            }
            if (javaNumber instanceof Long) {
                return new LispInt(((Long) javaNumber).longValue());
            }
            if (javaNumber instanceof Float) {
                return new LispInt(((Float) javaNumber).longValue());
            }
            if (javaNumber instanceof Double) {
                return new LispInt(((Double) javaNumber).longValue());
            }
        }

        throw new LispException("Object '%s' cannot be converted to int", object);
    }
}
