package kevwargo.jlp.objects.types;

import kevwargo.jlp.LispException;
import kevwargo.jlp.objects.LispBool;
import kevwargo.jlp.objects.LispObject;
import kevwargo.jlp.utils.ArgumentsIterator;
import kevwargo.jlp.utils.LispNamespace;


public class BoolType extends LispType {

    BoolType() {
        super("bool");
    }

    public LispObject makeInstance(LispNamespace namespace, ArgumentsIterator arguments) throws LispException {
        if (arguments.getLength() < 1) {
            return LispBool.FALSE;
        }
        return arguments.next() == LispBool.FALSE ? LispBool.FALSE : LispBool.TRUE;
    }

}
