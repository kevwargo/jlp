package kevwargo.jlp.object;

import java.util.HashMap;
import kevwargo.jlp.LispException;


public abstract class LispDataObject extends LispObject {

    public LispObject eval(HashMap<String, LispObject> namespace) throws LispException {
        return this;
    }
    
}
