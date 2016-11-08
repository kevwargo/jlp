package kevwargo.jlp.object;

import kevwargo.jlp.LispException;
import kevwargo.jlp.LispNamespace;


public abstract class LispDataObject extends LispObject {

    public LispObject eval(LispNamespace namespace) throws LispException {
        return this;
    }
    
}
