package kevwargo.jlp.objects.builtins.javareflect;

import java.util.Map;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFloat;
import kevwargo.jlp.objects.LispFunction;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.objects.types.LispType;
import kevwargo.jlp.utils.FormalArguments;
import kevwargo.jlp.utils.LispNamespace;


public class LFFloat extends LispFunction {

    public static final String NAME = "%float";
    public static final String ARG_NUMBER = "number";

    public LFFloat() {
        super(LispType.FUNCTION, NAME, new FormalArguments().pos(ARG_NUMBER));
    }

    protected LispObject callInternal(LispNamespace namespace, Map<String, LispObject> arguments) throws LispException {
        float value;
        LispObject number = arguments.get(ARG_NUMBER);

        if (number.isInstance(LispType.FLOAT)) {
            value = (float)((LispFloat)number.cast(LispType.FLOAT)).getValue();
        } else if (number.isInstance(LispType.INT)) {
            value = (float)((LispInt)number.cast(LispType.INT)).getValue();
        } else if (number.isInstance(LispType.STRING)) {
            try {
                value = Float.parseFloat(((LispString)number.cast(LispType.STRING)).getValue());
            } catch (NumberFormatException e) {
                throw new LispException(e);
            }
        } else {
            throw new LispException(String.format("Cannot cast '%s' to Java 'float'", number));
        }

        return new JavaFloat(value);
    }


    private static class JavaFloat extends LispJavaObject {

        private float value;

        JavaFloat(float value) {
            super(value);
            this.value = value;
        }

        public Object getJavaObject() {
            return value;
        }

        public Class<?> getJavaClass() {
            return Float.TYPE;
        }

    }

}
