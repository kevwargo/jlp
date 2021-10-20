package kevwargo.jlp.objects.builtins.javareflect;

import java.util.Map;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFloat;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class LFLong extends LispFunction {

    public static final String NAME = "%long";
    public static final String ARG_NUMBER = "number";

    public LFLong() {
        super(LispType.FUNCTION, NAME, new FormalArguments().pos(ARG_NUMBER));
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
        long value;
        LispObject number = arguments.get(ARG_NUMBER);

        if (number.isInstance(LispType.FLOAT)) {
            value = (long)((LispFloat)number.cast(LispType.FLOAT)).getValue();
        } else if (number.isInstance(LispType.INT)) {
            value = ((LispInt)number.cast(LispType.INT)).getValue();
        } else if (number.isInstance(LispType.STRING)) {
            try {
                value = Long.parseLong(((LispString)number.cast(LispType.STRING)).getValue());
            } catch (NumberFormatException e) {
                throw new LispException(e);
            }
        } else {
            throw new LispException(String.format("Cannot cast '%s' to Java 'long'", number));
        }

        return new JavaLong(value);
    }


    private static class JavaLong extends LispJavaObject {

        private long value;

        JavaLong(long value) {
            super(value);
            this.value = value;
        }

        public Object getJavaObject() {
            return value;
        }

        public Class<?> getJavaClass() {
            return Long.TYPE;
        }

    }

}
