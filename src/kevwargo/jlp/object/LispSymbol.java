package kevwargo.jlp.object;

import java.util.HashMap;
import kevwargo.jlp.LispException;


public class LispSymbol extends LispObject {

    private String name;

    public LispSymbol(String name) {
        this.name = name;
    }

    public LispObject eval(HashMap<String, LispObject> namespace) throws LispException {
        LispObject result = namespace.get(name);
        if (result == null) {
            throw new LispException(String.format("Global symbol %s not found", name));
        }
        return result;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String toString() {
        return String.format("LispSymbol %s", name);
    }

}
