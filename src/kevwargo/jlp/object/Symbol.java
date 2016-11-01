package kevwargo.jlp.object;

import java.util.HashMap;
import kevwargo.jlp.LispException;


public class Symbol extends LispObject {

    private String name;

    public Symbol(String name) {
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

}
