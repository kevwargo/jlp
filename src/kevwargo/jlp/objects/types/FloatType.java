package kevwargo.jlp.objects.types;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispFloat;
import kevwargo.jlp.objects.LispInt;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispString;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.LispNamespace;

public class FloatType extends LispType {

    FloatType() {
        super(LispType.TYPE, "float", new LispType[] { LispType.OBJECT });
    }

    public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        if (arguments.hasNext()) {
            LispObject object = arguments.next();
            if (object.isInstance(INT)) {
                return new LispFloat((double)((LispInt)object.cast(INT)).getValue());
            } else if (object.isInstance(FLOAT)) {
                return new LispFloat(((LispFloat)object.cast(FLOAT)).getValue());
            } else if (object.isInstance(STRING)) {
                return new LispFloat(Double.parseDouble(((LispString)object.cast(STRING)).getValue()));
            } else {
                throw new LispException(String.format("Object '%s' cannot be converted to float", object.toString()));
            }
        } else {
            return new LispFloat(0.0);
        }
    }

}
