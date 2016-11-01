package kevwargo.jlp.object;

import java.util.HashMap;
import java.util.List;
import kevwargo.jlp.LispException;


public abstract class LispFunction extends LispObject {

    public LispObject eval(HashMap<String, LispObject> namespace) throws LispException {
        return eval(namespace, null);
    }

    public abstract LispObject eval(HashMap<String, LispObject> namespace, List<LispObject> args) throws LispException;

}
