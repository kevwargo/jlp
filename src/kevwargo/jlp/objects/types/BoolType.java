package kevwargo.jlp.objects.types;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.LispNamespace;


public class BoolType extends LispType {

    BoolType() {
        super(LispType.TYPE, "bool", new LispType[] { LispType.OBJECT });
    }

    public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        if (arguments.getLength() != 1) {
            throw new LispException("bool() takes 1 argument");
        }
        return arguments.next() == LispBool.FALSE ? LispBool.FALSE : LispBool.TRUE;
    }

}
