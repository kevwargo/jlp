package kevwargo.jlp.object;

import kevwargo.jlp.LispException;
import kevwargo.jlp.LispNamespace;


public abstract class LispObject {

    public abstract LispObject eval(LispNamespace namespace) throws LispException;
    
}