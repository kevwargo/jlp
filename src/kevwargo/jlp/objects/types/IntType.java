package kevwargo.jlp.objects.types;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFloat;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.LispNamespace;

public class IntType extends LispType {

    IntType() {
        super("int");
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
