package kevwargo.jlp.objects;

import kevwargo.jlp.LispException;
import kevwargo.jlp.utils.LispNamespace;


public abstract class LispDataObject extends LispObject {

    public LispObject eval(LispNamespace namespace) throws LispException {
        return this;
    }
    
}
