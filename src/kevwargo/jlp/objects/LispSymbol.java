package kevwargo.jlp.objects;

import kevwargo.jlp.LispException;
import kevwargo.jlp.LispNamespace;


public class LispSymbol extends LispObject {

    private String name;

    public LispSymbol(String name) {
        this.name = name;
    }

    public LispObject eval(LispNamespace namespace) throws LispException {
        return namespace.resolve(name);
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String toString() {
        return String.format("'%s", name);
    }

}
