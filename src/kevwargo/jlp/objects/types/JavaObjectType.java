package kevwargo.jlp.objects.types;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.objects.LispJavaObject;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.LispNamespace;

public class JavaObjectType extends LispType {

    JavaObjectType() {
        super("java-object");
    }

    public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        if (arguments.hasNext()) {
            throw new LispException("java-object's constructor does not accept any arguments");
        }

        return new LispJavaObject(new Object());
    }

}
