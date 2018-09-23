package kevwargo.jlp.objects.types;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFloat;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.LispNamespace;

public class IntType extends LispType {

    IntType() {
        super(LispType.TYPE, "int", new LispType[] { OBJECT });
    }

    public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        if (arguments.hasNext()) {
            LispObject object = arguments.next();
            if (object.isInstance(INT)) {
                return new LispInt(((LispInt)object.cast(INT)).getValue());
            } else if (object.isInstance(FLOAT)) {
                return new LispInt((long)((LispFloat)object.cast(FLOAT)).getValue());
            } else if (object.isInstance(STRING)) {
                int radix = 10;
                if (arguments.hasNext()) {
                    radix = (int)((LispInt)arguments.next().cast(INT)).getValue();
                }
                return new LispInt(Long.parseLong(((LispString)object.cast(STRING)).getValue(), radix));
            } else {
                throw new LispException(String.format("Object '%s' cannot be converted to int", object.toString()));
            }
        } else {
            return new LispInt(0);
        }
    }

}
