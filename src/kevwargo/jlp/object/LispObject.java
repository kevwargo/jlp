package kevwargo.jlp.object;

import java.util.HashMap;
import kevwargo.jlp.LispException;


public abstract class LispObject {

    public abstract LispObject eval(HashMap<String, LispObject> namespace) throws LispException;
    
}
