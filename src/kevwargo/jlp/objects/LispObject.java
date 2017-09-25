package kevwargo.jlp.objects;

import java.util.Iterator;
import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.LispNamespace;


public abstract class LispObject {

    public abstract LispObject eval(LispNamespace namespace) throws LispException;

    public abstract String type();


    public LispObject call(LispNamespace namespace, Iterator<LispObject> arguments) throws LispException {
        throw new LispException(String.format("`%s' object is not a function", type()));
    }

    public LispObject assertType(String type) throws LispException {
        if (!type().equals(type)) {
            throw new LispException(this + " is not of type " + type);
        }
        return this;
    }

    public boolean getBooleanValue() {
        return true;
    }
    
}
