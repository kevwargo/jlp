package kevwargo.jlp.objects;

import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.LispNamespace;


public abstract class LispObject {

    public abstract LispObject eval(LispNamespace namespace) throws LispException;

    public boolean getBooleanValue() {
        return true;
    }
    
}
