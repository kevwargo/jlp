package kevwargo.jlp.objects.types;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFloat;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.LispNamespace;

public class FloatType extends LispType {

    FloatType() {
        super("float");
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
